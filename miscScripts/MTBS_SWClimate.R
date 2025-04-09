# SW Wildfire analysis - using MTBS perimeters for climate extracts, other regions
# MAC 12/5/23
# adapted from burn period research regionsMTBS_SWClimate.R, fodAnalysis.R, FOD_SWClimate.R, regionsFOD_SWClimate.R
# using data from https://www.mtbs.gov/direct-download

##### CREATE MTBS dataframe with ecoregions, vegtypes -----
#library(tidyverse)
#library(ggplot2)
# load/filter mtbs perims, ./BurnPeriodResearch/mtbs_perims.R 
#  load("~/RProjects/BurnPeriodResearch/data/AZNM_mtbs_perims_1984_2022.RData")
# 
# # remove factors -- from ./BurnPeriodResearch/fireProgression_loop.R
#  perimsCrop$BurnBndAc<-as.numeric(as.character(perimsCrop$BurnBndAc))
#  perimsCrop$Incid_Name<-as.character(perimsCrop$Incid_Name)
#  perimsCrop$Ig_Date<-as.Date(perimsCrop$Ig_Date, "%Y-%m-%d")
#  perimsCrop$year<-as.numeric(format(as.Date(perimsCrop$Ig_Date, format="%Y/%m/%d"),"%Y"))
#  perimsCrop<-subset(perimsCrop, Incid_Type!="Prescribed Fire")
# 
# # map layers
# states <- raster::getData('GADM', country='United States', level=1)
# aznm<-subset(states, NAME_1=="Arizona" | NAME_1=="New Mexico")
# ecoreg<-rgdal::readOGR(dsn="~/RProjects/SOMs/monsoonPrecip/shapes", layer="us_eco_l3")
# ecoreg <- sp::spTransform(ecoreg,raster::crs(states))
# # intersect polys
# ecoreg<-raster::intersect(ecoreg,aznm)
# 
# #  add in ecoreg to points
# #  https://gis.stackexchange.com/questions/270537/joining-attributes-by-location-for-more-than-one-located-feature-using-r
# ecoreg <- sp::spTransform(ecoreg,raster::crs(perimsCrop))
# 
# # # us sf to join
# sf::sf_use_s2(FALSE)
#  perimsCrop <- sf::st_join(sf::st_as_sf(perimsCrop), sf::st_as_sf(ecoreg), left=TRUE)
#  perimsCrop<-perimsCrop[!duplicated(perimsCrop$Event_ID),]
#  perimsCrop <- subset(perimsCrop, !is.na(US_L3CODE))
# 
# # drop some columns
# perimsCrop <- perimsCrop[,c(1:4,6,8:11,23,25,40)]
# 
# ##### GROUPVEG using BPS https://landfire.gov/bps.php
# # read in landfire data, use Terra library
# #bps<-raster("./data/landfire/LF2020_BPS_220_CONUS/LC20_BPS_220.tif")
# bps<-terra::rast("./data/landfire/LF2020_BPS_220_CONUS/LC20_BPS_220.tif")
#   bps<- terra::project(bps, "EPSG:4326", method = "near")
# # extract BPS name and groupveg
# perimsCrop$groupVeg<-exactextractr::exact_extract(bps,perimsCrop,'mode')
#   bpsMeta<- read.csv('https://landfire.gov/CSV/LF2020/LF20_BPS_220.csv')
#   perimsCrop<-merge(perimsCrop, bpsMeta[,c(1,6)], by.x="groupVeg",by.y="VALUE")
# ##### end bps

##### GROUPVEG using EVT https://landfire.gov/evt.php
# read in landfire data, use Terra library
# evt<-terra::rast("./data/landfire/LF2022_EVT_230_CONUS/LC22_EVT_230.tif")
#   evt<- terra::project(evt, "EPSG:4326", method = "near")
# 
# # extract BPS name and groupveg
# perimsCrop$groupVeg<-exactextractr::exact_extract(evt,perimsCrop,'mode')
#   evtMeta<- read.csv('https://landfire.gov/CSV/LF2022/LF22_EVT_230.csv')
# perimsCrop<-merge(perimsCrop, evtMeta[,c(1,7)], by.x="groupVeg",by.y="VALUE")
##### end evt

#save(perimsCrop,ecoreg, aznm, file="./data/mtbsSW_ecoreg.RData")
#####

##### mtbs diagnostic plots/summaries----
# yearFire<-perimsCrop %>%
#   group_by(year) %>%
#   summarise(totAc=sum(BurnBndAc),
#             nFires=n())
# 
# ggplot(yearFire, aes(year,nFires))+
#   geom_bar(stat = "identity")
# 
# perimsCropSF <- sf::st_as_sf(perimsCrop)
# ggplot()+
#   geom_sf(data=perimsCropSF, aes(fill=as.factor(year)), color='black', linewidth=0.5, alpha=1)+
#   coord_sf()+
#   ggtitle("MTBS Wildfires, 1984-2022")
#####

##### extract MTBS climate time series -----
# 
# library(SPEI)
# library(SCI)
# library(tidyverse)
# 
# load("~/RProjects/SWWildfires/data/mtbsSW_ecoreg.RData")
# 
# #load PRISM data, adapted from ./WinterSummerPrecip/extractClusterTS.R
# #dates 1895-2022 PRISM data
# dates=seq(as.Date("1895-01-01"), as.Date("2022-12-31"), by="month")
# dates<-as.data.frame(dates)
# dates$month<-as.numeric(format(dates$dates, "%m"))
# dates$year<-as.numeric(format(dates$dates, "%Y"))
# length(unique(dates$year))
# 
# # update scratch dir with PRISM data 1895-2022 monthly precip/mean temp
# # use ~/RProjects/PRISMDownload/monthyDownloadPRISM.R
# # process to subset using ~/RProjects/WinterSummerPrecip/processPRISM.R
# climFiles<-as.data.frame(list.files(path = "/scratch/crimmins/PRISM/monthly/processed/SW", pattern="*.grd",full.names = TRUE, recursive = TRUE))
# climFiles$fileName<-(list.files(path = "/scratch/crimmins/PRISM/monthly/processed/SW", pattern="*.grd",full.names = FALSE, recursive = TRUE))
# colnames(climFiles)<-c("path","name")
# climFiles<-climFiles %>%
#   separate(name, sep="_", into=c("dataset", "var", "being","end"))
# climFiles<-subset(climFiles, end=="2022.grd")
# climFiles$path<-as.character(climFiles$path)
# ###
# # extract time series from fire perimeter
# climAnom<-list()
# tictoc::tic()
# mtbsClim<-list()
# for(j in 1:nrow(perimsCrop)){
# 
#   print(j)
# 
#   climTS<-list()
#   for(i in 1:nrow(climFiles)){
#     # temp raster var
#     #var<-stack(climFiles$path[i])
#     var<-terra::rast(climFiles$path[i])
#     # extract time series
#     ext<-as.data.frame(t(terra::extract(var, terra::vect(perimsCrop[j,]), ID=FALSE, raw=FALSE, fun=mean)))
#     ext<-as.data.frame(ext[2:nrow(ext),])
#     colnames(ext)<-climFiles$var[i]
#     climTS[[i]]<-ext
#     #print(climFiles$var[i])
#   }
#   # create fire dataframe
#   climTS<-do.call(cbind, climTS)
#   climTS<-cbind.data.frame(perimsCrop$Event_ID[j],
#                            dates,climTS)
#   colnames(climTS)[1]<-c("eco1")
# 
#   # subset to different period of record
#   climTS<-subset(climTS, year<=2021)
#   
#   # calculate SPI using SPEI package
#   # climTS$spi3<-spi(climTS$prec,3, na.rm = TRUE)$fitted
#   # climTS$spi6<-spi(climTS$prec,6, na.rm = TRUE)$fitted
#   # climTS$spi12<-spi(climTS$prec,12, na.rm = TRUE)$fitted
#   # climTS$spi24<-spi(climTS$prec,24, na.rm = TRUE)$fitted
# 
#   # calculate SPI with SCI package
#   spi.para<-fitSCI(climTS$prec,first.mon=1,distr="gamma",time.scale=3,p0=TRUE)
#     climTS$spi3<-transformSCI(climTS$prec,first.mon=1,obj=spi.para,sci.limit=4)
#   spi.para<-fitSCI(climTS$prec,first.mon=1,distr="gamma",time.scale=6,p0=TRUE)
#     climTS$spi6<-transformSCI(climTS$prec,first.mon=1,obj=spi.para,sci.limit=4)
#   spi.para<-fitSCI(climTS$prec,first.mon=1,distr="gamma",time.scale=12,p0=TRUE)
#     climTS$spi12<-transformSCI(climTS$prec,first.mon=1,obj=spi.para,sci.limit=4)
#   spi.para<-fitSCI(climTS$prec,first.mon=1,distr="gamma",time.scale=24,p0=TRUE)
#     climTS$spi24<-transformSCI(climTS$prec,first.mon=1,obj=spi.para,sci.limit=4)
# 
#   # calculate SPEI using SPEI package
#   climTS$spei3<-spei(climTS$prec-climTS$hargreaves,3, na.rm=TRUE)$fitted
#   climTS$spei6<-spei(climTS$prec-climTS$hargreaves,6, na.rm=TRUE)$fitted
#   climTS$spei12<-spei(climTS$prec-climTS$hargreaves,12, na.rm=TRUE)$fitted
#   climTS$spei24<-spei(climTS$prec-climTS$hargreaves,24, na.rm=TRUE)$fitted
# 
#   # 3-month moving sums/avgs
#   clim3mo<-cbind.data.frame(zoo::rollapply(climTS[,c("hargreaves","prec")], FUN = sum, width = 3,
#                                            fill=NA,align="right", by.column = TRUE),
#                             zoo::rollapply(climTS[,c("tdmean","tmax","tmean","tmin","vpdmax")], FUN = mean, width = 3,
#                                            fill=NA,align="right", by.column = TRUE))
#   #colnames(clim3mo)<-paste0(colnames(clim3mo),"3mo")
# 
#   # calculate z-scores
#   clim3moZ<-cbind.data.frame(climTS$dates,climTS$month,clim3mo)
#   colnames(clim3moZ)[1:2]<-c("dates","month")
#   clim3moZ<-clim3moZ %>%
#     group_by(month) %>%
#     mutate(tmeanZ = (tmean-mean(tmean, na.rm=TRUE))/sd(tmean, na.rm=TRUE),
#            vpdmaxZ = (vpdmax-mean(vpdmax, na.rm=TRUE))/sd(vpdmax, na.rm=TRUE),
#            hargZ = (hargreaves-mean(hargreaves, na.rm=TRUE))/sd(hargreaves, na.rm=TRUE),
#            tdmeanZ = (tdmean-mean(tdmean, na.rm=TRUE))/sd(tdmean, na.rm=TRUE),
#            tmaxZ = (tmax-mean(tmax, na.rm=TRUE))/sd(tmax, na.rm=TRUE),
#            tminZ = (tmin-mean(tmin, na.rm=TRUE))/sd(tmin, na.rm=TRUE))
#   clim3moZ<-clim3moZ[,c("dates","tmeanZ","vpdmaxZ","hargZ","tdmeanZ","tmaxZ","tminZ")]
# 
#   # 3-month anoms, swap in rolling avg/sum
#   anoms<-cbind.data.frame(climTS$dates,climTS$month,perimsCrop$Event_ID[j],clim3mo)
#   colnames(anoms)[1:3]<-c("dates","month","eco1")
#   # calculate anomalies
#   #anoms<-climTS[,6:14]
#   meanMO<- anoms %>% group_by(month) %>%
#     summarise(meanHargreaves=mean(hargreaves, na.rm=TRUE),
#               meanPrec=mean(prec, na.rm=TRUE),
#               meanTDmean=mean(tdmean, na.rm=TRUE),
#               meanTmax=mean(tmax, na.rm=TRUE),
#               meanTmean=mean(tmean, na.rm=TRUE),
#               meanTmin=mean(tmin, na.rm=TRUE),
#               meanVPDmax=mean(vpdmax, na.rm=TRUE))
#   anoms<-merge(anoms,meanMO, by=c("month"))
#   # calculate anoms
#   anoms$anomHarg<-anoms$hargreaves-anoms$meanHargreaves
#   anoms$anomPrec<-anoms$prec-anoms$meanPrec
#   anoms$anomTDmean<-anoms$tdmean-anoms$meanTDmean
#   anoms$anomTmax<-anoms$tmax-anoms$meanTmax
#   anoms$anomTmean<-anoms$tmean-anoms$meanTmean
#   anoms$anomTmin<-anoms$tmin-anoms$meanTmin
#   anoms$anomVPDmax<-anoms$vpdmax-anoms$meanVPDmax
#   anoms <- anoms[order(anoms$dates),]
# 
#   # add spi back in
#   anoms<-merge(anoms,climTS[,c("dates","spi3","spi6","spi12","spi24",
#                                "spei3","spei6","spei12","spei24")], by=c("dates"))
# 
#   # add z-scores back in
#   anoms<-merge(anoms, clim3moZ, by="dates")
# 
#   # fixed 3-mo seasons
#   anoms$seas<-cut(anoms$month,c(0,3,6,9,12))
#   levels(anoms$seas) = c("JFM","AMJ","JAS","OND")
#   anoms<-subset(anoms, month %in% c(3,6,9,12))
# 
#   # subset to recent decades
#   anoms<-subset(anoms, dates>="1980-01-01")
# 
#   # write to list
#   mtbsClim[[j]]<-anoms
# 
#   # save full anoms
#   #climAnom[[j]]<-anoms
# 
# }
# tictoc::toc()
# 
# save(mtbsClim, file="./data/AZNM_mtbs_fires_climate_3moRoll.RData")
#####


##### analyze fires and mtbs fireperim climate ----
library(tidyverse)
library(ggplot2)
library(cowplot)

## function
rowSd <- function (x, na.rm=FALSE) apply(X=x, MARGIN=1, FUN=sd, na.rm=na.rm)
##

load("~/RProjects/SWWildfires/data/mtbsSW_ecoreg.RData")
# see regionsMTBS_SWClimate.R
#load("./data/AZNM_ecoregion_climate_3moRoll.RData")
load("~/RProjects/SWWildfires/data/AZNM_mtbs_fires_climate_3moRoll.RData")

# filter out border state fires based on event ID
perimsCrop$event_st<-substr(perimsCrop$Event_ID,1,2)
perimsCrop<-subset(perimsCrop, event_st %in% c("AZ","NM"))

# drop fires without groupVeg
#sum(perimsCrop$BurnBndAc[perimsCrop$GROUPVEG=="Fill-Not Mapped"])/sum(perimsCrop$BurnBndAc)
perimsCrop<-subset(perimsCrop, !(GROUPVEG %in% c("Fill-Not Mapped")))

### subset to fire sizes
#perimsCrop<-subset(perimsCrop, BurnBndAc>=10000)
# thin mtbsClim to subset fires
# fireID<-c()
# for(i in 1:length(mtbsClim)){
#   fireID[i]<-as.character(mtbsClim[[i]]$eco1[1])
# }
# mtbsClim<-mtbsClim[which(fireID %in% perimsCrop$Event_ID)]
#mtbsClim<-do.call(rbind, mtbsClim)

# add in fire seasons
perimsCrop$month<-as.numeric(format(perimsCrop$Ig_Date,"%m"))
perimsCrop$seas<-cut(perimsCrop$month,c(0,3,6,9,12))
levels(perimsCrop$seas) = c("JFM","AMJ","JAS","OND")

# add in fire size class
perimsCrop$fireSizeClass<-cut(perimsCrop$BurnBndAc, c(0,4999,9999,49999,99999,499999,Inf))
levels(perimsCrop$fireSizeClass)<-c("F","G","H","I","J","K")
#table(mtbsDF$fireSizeClass)
perimsCrop$fireSizeClass2<-cut(perimsCrop$BurnBndAc, c(0,4999,Inf))
levels(perimsCrop$fireSizeClass2)<-c("F","G+")
table(perimsCrop$fireSizeClass2)

# create DF
mtbsDF<-sf::st_drop_geometry(perimsCrop)
# droplevels
mtbsDF<-droplevels(mtbsDF)
#mtbsDF$month<-as.numeric(format(mtbsDF$Ig_Date,"%m"))
#mtbsDF$seas<-cut(mtbsDF$month,c(0,3,6,9,12))
#levels(mtbsDF$seas) = c("JFM","AMJ","JAS","OND")


# identify if there are reburn areas over different years
# https://gis.stackexchange.com/questions/437047/identify-overlapping-polygons-within-a-single-multipolygon

##### process antecedent climate ----
# fireClim<-list()
# # gather antecedent climate for each fire
# for(i in 1:nrow(mtbsDF)){
# 
#   tempClim<-mtbsClim[[i]]
#   tempFire<-mtbsDF[i,]
# 
#   # assign fire month to fixed season
#   fireMo<-as.numeric(format(mtbsDF$Ig_Date[i], "%m"))
#   fireMo<-ifelse(fireMo==12 & fireMo>=10, 12,
#                  ifelse(fireMo>=7 & fireMo<=9, 9,
#                         ifelse(fireMo>=4 && fireMo<=6, 6,3)))
#   fireDate<-as.Date(paste0(format(mtbsDF$Ig_Date[i], "%Y"),"-",fireMo,"-01"))
#   # subset to fire event
#   firePrior<-lubridate::ymd(fireDate)-lubridate::years(2)
#   temp<-subset(tempClim, dates>=firePrior & dates<=fireDate) ## need to assign/round month to last mo of season, 5 goes to 6/1/xxxx
#   temp$seq<-seq(1,nrow(temp),by=1)+(-nrow(temp))
#   temp$seasSeq<-paste0(temp$seas,"-",abs(temp$seq))
# 
#   # add fire info to climate DF
#   temp$fireSeas<-tempFire$seas
#   temp$fireSize<-tempFire$BurnBndAc
#   temp$fireName<-tempFire$Incid_Name
#   temp$fireYear<-tempFire$year
#   temp$ecoName<-tempFire$US_L3NAME
#   temp$groupVeg<-tempFire$GROUPVEG
#   temp$fireSizeClass<-tempFire$fireSizeClass
#   temp$fireSizeClass2<-tempFire$fireSizeClass2
# 
#   # create non fire years climatology
#   seqSeas<-temp[,c("seas","seq")]
#   tempClim<-merge(tempClim, temp[,c("seas","seq")], by="seas", all.x=TRUE)
#   tempClim$seasSeq<-paste0(tempClim$seas,"-",abs(tempClim$seq))
#   # subset out ante Fire years
#   tempClim<-subset(tempClim, dates<firePrior | dates>fireDate)
#   tempClim<-tempClim[order(tempClim$dates),]
# 
#   # create random distribution
#   seas0<-which(tempClim$seq==0)
#   #idx<-sample(seq(1:length(seas0)),1)
# 
#   cSPI<-list()
#   cSPEI<-list()
#   cTmean<-list()
#   cVPD<-list()
#   nlst<-1
#   for(l in 1:length(seas0)){
# 
#     idx1<-seas0[l]
#     idx2<-seas0[l]-20
#     if(idx2>0){
#       tempClimSub<-tempClim[c(idx1:idx2),]
#       tempClimSub<-tempClimSub[order(tempClimSub$dates),]
# 
#       #3 get sequence index
#       seqIdx<-c()
#       k=1
#       seq8<-seq(-8,0,1)
#       for(j in 1:nrow(tempClimSub)){
#         if(k<=length(seq8)){
#           if(seq8[k]==tempClimSub$seq[j]){
#             seqIdx[k]<-j
#             k=k+1
#           }
#         }
#       }
#       ##
#       tempClimSub<-tempClimSub[seqIdx,]
#       if(nrow(tempClimSub)==length(seq8)){
#         cSPI[[nlst]]<-tempClimSub$spi3
#         cSPEI[[nlst]]<-tempClimSub$spei3
#         cVPD[[nlst]]<-tempClimSub$anomVPDmax
#         cTmean[[nlst]]<-tempClimSub$anomTmean
#         nlst<-nlst+1
#       }
#     }
#   }
#   # create means/sd
#   cSPI<-do.call(cbind,cSPI)
#     temp$meanSPI3NF<-rowMeans(cSPI)
#     temp$sdSPI3NF<-rowSd(cSPI)
#   cSPEI<-do.call(cbind,cSPEI)
#     temp$meanSPEI3NF<-rowMeans(cSPEI)
#     temp$sdSPEI3NF<-rowSd(cSPEI)
#   cVPD<-do.call(cbind,cVPD)
#     temp$meanVPDmaxNF<-rowMeans(cVPD)
#     temp$sdVPDmaxNF<-rowSd(cVPD)
#   cTmean<-do.call(cbind,cTmean)
#     temp$meanTmeanNF<-rowMeans(cTmean)
#     temp$sdTmeanNF<-rowSd(cTmean)
# 
#   fireClim[[i]]<-temp
#   print(i)
# }
# 
# fireClim<-do.call(rbind, fireClim)
# 
# saveRDS(fireClim, file = "./data/fireClim.Rds")
#####

# reload fireClim data frame
fireClim <- readRDS("~/RProjects/SWWildfires/data/fireClim.Rds")
#fireClim <- subset(fireClim, fireSize>=10000)

##### summary stats ----
# map of ecoreg
ecoregSF <- sf::st_as_sf(ecoreg)
ggplot()+
  geom_sf(data=ecoregSF, aes(), color='grey75', linewidth=0.5, alpha=1)+
  geom_sf(data=subset(perimsCrop, GROUPVEG %in% c("Conifer","Shrubland","Grassland")),
          aes(fill=as.factor(GROUPVEG)), color='black', linewidth=0.25, alpha=1)+
  coord_sf()+
  ggtitle("MTBS Fire Locations, 1984-2021")
#plotly::ggplotly(p)

##### veg group proportions, stats 
temp<-subset(mtbsDF, GROUPVEG %in% c("Conifer","Shrubland","Grassland"))
temp<-temp %>%
  group_by(GROUPVEG, US_L3NAME) %>%  # US_L3NAME
  summarise(nFires=n(),
            totAc=sum(BurnBndAc))%>%
  ungroup()
temp<-temp%>%
  mutate(nFires_perc=100 *(nFires/sum(nFires)),
         totAc_perc=100 *(totAc/sum(totAc)))
temp<-temp %>%
  group_by(GROUPVEG) %>%  # US_L3NAME
  mutate(nFires_perc_veg=100 *nFires/sum(nFires),
          totAc_perc_veg=100 *totAc/sum(totAc))

temp$ratio<-temp$totAc/temp$nFires
ggplot(temp, aes(x=US_L3NAME, y=totAc, fill=GROUPVEG))+
  geom_bar(width = 0.75, stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Total Acres Burned by Ecoregion")

ggplot(temp, aes(x=GROUPVEG, y=totAc, fill=US_L3NAME))+
  geom_bar(width = 0.75, stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Total Acres Burned by Ecoregion")

ggplot(temp, aes(x=GROUPVEG, y=nFires, fill=US_L3NAME))+
  geom_bar(width = 0.75, stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Total Acres Burned by Ecoregion")


temp<-subset(mtbsDF, GROUPVEG %in% c("Conifer","Shrubland","Grassland"))
tapply(temp$BurnBndAc, temp$GROUPVEG, summary)
#####

##### fire-veg yearly -----
# fire-veg yearly
temp2<-mtbsDF %>%
  group_by(year) %>%
  summarise(nFires=n(),
            totAc=sum(BurnBndAc)) %>%
  ungroup()
temp2$ratio<-temp2$totAc/temp2$nFires
temp2<-temp2%>%
  mutate(nFires_perc=100 *(nFires/sum(nFires)),
         totAc_perc=100 *(totAc/sum(totAc)))

temp<-mtbsDF %>%
  group_by(year, GROUPVEG) %>%
  summarise(nFires=n(),
            totAc=sum(BurnBndAc))
temp$ratio<-temp$totAc/temp$nFires
temp<- gather(temp, var, value, nFires:ratio, factor_key=TRUE)

ggplot(subset(temp, GROUPVEG %in% c("Conifer","Grassland","Shrubland")),
       aes(year,value,fill=GROUPVEG))+
  geom_bar(position="stack", stat="identity")+
  facet_wrap(.~var, ncol=1, scales="free")+
  ggtitle("Annual fire stats by veg type - stacked")

ggplot(subset(temp, GROUPVEG %in% c("Conifer","Grassland","Shrubland")),
       aes(year,value,fill=GROUPVEG))+
  geom_bar(position="stack", stat="identity")+
  #facet_wrap(.~var, ncol=1, scales="free")
  facet_grid(var~GROUPVEG, scales="free")+
  geom_smooth(method = "lm")
###


#####
# fire event months
# ggplot(mtbsDF, aes(month, color=US_L3NAME))+
#   #geom_histogram(bins=12, position="dodge")+
#   geom_freqpoly(binwidth = 1)+
#   ggtitle("Fire Counts/month")+
#   facet_wrap(.~GROUPVEG)
#####

##### fire event seasons -----
temp<-subset(mtbsDF, GROUPVEG %in% c("Conifer","Shrubland","Grassland"))
temp<-temp %>%
  group_by(GROUPVEG,seas) %>%
  summarise(nFires=n(),
            totAc=sum(BurnBndAc))
#temp$ratio<-temp$totAc/temp$nFires
temp<- gather(temp, var, value, nFires:totAc, factor_key=TRUE)
ggplot(temp, aes(x=seas, y=value, fill=GROUPVEG))+
  #geom_bar(width = 0.75, stat = "identity")+
  geom_bar(position="dodge", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(.~var, ncol=1, scales="free")+
  ggtitle("Fire Seasons")

temp<-subset(mtbsDF, GROUPVEG %in% c("Conifer","Shrubland","Grassland"))
temp<-temp %>%
  group_by(seas) %>%
  summarise(nFires=n(),
            totAc=sum(BurnBndAc)) %>%
  mutate(percFires=nFires/sum(nFires),
         percAc=totAc/sum(totAc))


#####

##### fire seas/year -----
temp<-mtbsDF %>%
  group_by(year, seas,GROUPVEG) %>%
  summarise(nFires=n(),
            totAc=sum(BurnBndAc))
temp$ratio<-temp$totAc/temp$nFires
temp<- gather(temp, var, value, nFires:ratio, factor_key=TRUE)

ggplot(subset(temp, GROUPVEG %in% c("Conifer","Grassland","Shrubland") & 
              seas %in% c("AMJ","JAS","OND","JFM")),
       aes(year,value,fill=seas))+
  geom_bar(position="stack", stat="identity")+
  #facet_wrap(.~var, ncol=1, scales="free")
  facet_grid(var~GROUPVEG, scales="free")+
  ggtitle("Annual fire stats by seas~vegType")
#####

##### fire veg/year --- compare veg to all ----
temp<-mtbsDF %>%
  group_by(year,GROUPVEG) %>%
  summarise(nFires=n(),
            totAc=sum(BurnBndAc))
# add in all group stats
temp2<- mtbsDF %>%
  group_by(year) %>%
  summarize(nFires=n(),
            totAc=sum(BurnBndAc)) %>%
  mutate( GROUPVEG = 'All' ) %>%
  ungroup() %>%
  rbind( ungroup(temp) ) %>%
  mutate( GROUPVEG = as.factor(GROUPVEG) )

temp2$ratio<-temp2$totAc/temp2$nFires
temp2<- gather(temp2, var, value, nFires:ratio,-GROUPVEG, factor_key=TRUE)

ggplot(subset(temp2, GROUPVEG %in% c("Conifer","Grassland","Shrubland","All")),
       aes(year,value,fill=GROUPVEG))+
  geom_bar(position="stack", stat="identity")+
  #facet_wrap(.~var, ncol=1, scales="free")
  facet_grid(var~GROUPVEG, scales="free")+
  ggtitle("Annual fire stats by vegType")+
  geom_smooth(method="lm")
#####

##### fire veg/year --- compare veg to all - with fireSizeClass2 ----
temp<-mtbsDF %>%
  group_by(year,GROUPVEG, fireSizeClass2) %>%
  summarise(nFires=n(),
            totAc=sum(BurnBndAc))
# add in all group stats
temp2<- mtbsDF %>%
  group_by(year, fireSizeClass2) %>%
  summarize(nFires=n(),
            totAc=sum(BurnBndAc)) %>%
  mutate( GROUPVEG = 'All' ) %>%
  ungroup() %>%
  rbind( ungroup(temp) ) %>%
  mutate( GROUPVEG = as.factor(GROUPVEG) )

temp2$ratio<-temp2$totAc/temp2$nFires
temp2<- gather(temp2, var, value, nFires:ratio,-GROUPVEG, factor_key=TRUE)

ggplot(subset(temp2, GROUPVEG %in% c("Conifer","Grassland","Shrubland","All")),
       aes(year,value,fill=fireSizeClass2))+
  geom_bar(position="stack", stat="identity")+
  #facet_wrap(.~var, ncol=1, scales="free")
  facet_grid(var~GROUPVEG, scales="free")+
  ggtitle("Annual fire stats by vegType - with FireSize Class")+
  geom_smooth(method="lm", se=T)
#####

##### seasonal fire-climate analysis scatterplots/time series -----
ggplot(subset(fireClim, seq==0 & groupVeg %in% c("Conifer","Grassland","Shrubland") &
                seas %in% c("AMJ","JAS")),
       aes(groupVeg,tmeanZ, fill=groupVeg))+
  geom_violin()+
  geom_boxplot(width=0.1)+
  #geom_boxplot(outlier.colour = NA, varwidth = FALSE)+
  ylim(-3,3)+
  geom_hline(yintercept = 0)+
  facet_grid(fireSizeClass2~fireSeas)+
  ggtitle("Fire Season Climate Distribution")

#mid<-median(log(fireClim$fireSize))
give.cor <- function(x){
  return(c(y = mean(x), label = cor(x,y)))
}

p1<- ggplot(subset(fireClim, seq==0 & groupVeg %in% c("Conifer","Grassland","Shrubland")),
       aes(tdmeanZ, vpdmaxZ, color=as.factor(fireSizeClass2)))+
  geom_point(alpha=0.50)+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  #geom_smooth(method = lm)+
  facet_grid(groupVeg~fireSeas)+
  #stat_summary(fun.data = give.cor, geom = "text")+
  #scale_color_gradient(low="#fee391", high="#662506")+
  ggtitle("tdmean-Z vs vpdmax-Z")
plotly::ggplotly(p1)

ggplot(subset(fireClim, seq==0 & groupVeg %in% c("Conifer","Grassland","Shrubland")),
              aes(spi3, vpdmaxZ)) +
  geom_hex(bins = 15) +
  scale_fill_continuous(type = "viridis") +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  theme_bw()+
  facet_wrap(groupVeg~fireSeas)

ggplot(subset(fireClim, groupVeg %in% c("Conifer","Grassland","Shrubland") & seq==0 & fireSeas=="AMJ"),
       aes(as.factor(fireYear),spei3,fill=fireSeas))+
  geom_boxplot(varwidth = TRUE, outlier.colour = NA)+
  geom_hline(yintercept = 0)+
  facet_wrap(.~groupVeg, ncol=1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(subset(fireClim, groupVeg %in% c("Conifer","Grassland","Shrubland") & seq==0 & fireSeas=="AMJ"),
       aes(as.factor(fireYear),spi3,color=fireSizeClass))+
 geom_point()+
  geom_hline(yintercept = 0)+
  facet_wrap(.~groupVeg, ncol=1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  #scale_color_gradient(low="#fee391", high="#662506")+
  #scale_color_gradient2(low="brown", high="green", mid="grey", midpoint = 0)
  ggtitle("Fire Event SPI - AMJ Fires")
#####

##### antecedent seasonal fire-climate analyis - composite lines ----
ggplot(subset(fireClim, fireSeas=="AMJ"),
       aes(seq,spi3,color=as.factor(fireYear), group=eco1))+
  geom_line(alpha=0.5)+
  geom_hline(yintercept = 0)+
  facet_wrap(.~groupVeg)+
  ggtitle("composite antecedent seasonal climate")

####
# ggplot(subset(fireClim, fireSeas=="AMJ"),
#        aes(seq,spi3, group=seq))+
#   #geom_line(alpha=0.5)+
#   geom_boxplot(varwidth = TRUE)+
#   geom_hline(yintercept = 0)+
#   facet_grid(groupVeg~ecoName)
####

####
# ggplot(subset(fireClim, groupVeg %in% c("Conifer","Grassland","Shrubland") & seq>=-7 & fireSeas %in% c("AMJ","JAS","OND","JFM")),
#        aes(as.factor(seq),spei3,fill=ecoName))+
#   geom_boxplot(varwidth = FALSE, outlier.colour = NA)+
#   geom_hline(yintercept = 0)+
#   facet_wrap(.~fireSeas, ncol=1)
#####

##### antecedent seasonal fire-climate -- boxplots -----
ggplot(subset(fireClim, groupVeg %in% c("Conifer","Grassland","Shrubland") & seq>=-4 & fireSeas %in% c("AMJ","JAS","OND","JFM")),
       aes(as.factor(seq),spi3,fill=groupVeg))+
  geom_boxplot(varwidth = TRUE, outlier.colour = NA)+
  geom_hline(yintercept = 0)+
  ylim(-3,3)+
  facet_wrap(.~fireSeas, ncol=1)+
  ggtitle("antecedent seasonal fire climate")

ggplot(subset(fireClim, groupVeg %in% c("Conifer","Grassland","Shrubland") & seq>=-4 & fireSeas %in% c("AMJ","JAS")),
       aes(as.factor(seq),spi3,fill=groupVeg))+
  geom_boxplot(varwidth = TRUE, outlier.colour = NA)+
  geom_hline(yintercept = 0)+
  ylim(-4,4)+
  facet_grid(fireSeas~fireSizeClass2)+
  ggtitle("antecedent seasonal fire climate")

levels(fireClim$ecoName)

ggplot(subset(fireClim, groupVeg %in% c("Conifer","Grassland","Shrubland")
              & seq>=-4
              & fireSeas %in% c("AMJ","JAS")
              & ecoName == "Sonoran Basin and Range"),
       aes(as.factor(seq),spi3,fill=groupVeg))+
  geom_boxplot(varwidth = TRUE, outlier.colour = NA)+
  geom_hline(yintercept = 0)+
  ylim(-3,3)+
  facet_grid(fireSeas~fireSizeClass2)+
  ggtitle("antecedent seasonal fire climate")

p<-ggplot(subset(fireClim, groupVeg %in% c("Shrubland","Conifer","Grassland")
              & seq>=-4
              & fireSeas %in% c("AMJ","JAS")),
            #  & ecoName == "Madrean Archipelago"),
       aes(seq,spei3,color=as.factor(fireYear), group=eco1))+
  geom_line(alpha=0.5)+
  geom_hline(yintercept = 0)+
  facet_grid(fireSeas~groupVeg)+
  ggtitle("composite antecedent seasonal climate")

plotly::ggplotly(p)

p<-ggplot(subset(fireClim, groupVeg %in% c("Shrubland","Conifer","Grassland")
              & seq>=-4
              & fireSeas %in% c("AMJ","JAS")
              & fireYear==2011
              & fireSizeClass2 %in% c("G+","F")),
       #  & ecoName == "Madrean Archipelago"),
       aes(seq,spei3,color=ecoName, group=eco1))+
  geom_line(alpha=0.5)+
  geom_hline(yintercept = 0)+
  facet_grid(fireSeas~groupVeg)+
  ggtitle("composite antecedent seasonal climate - 2011")
plotly::ggplotly(p)
#####

##### antecedent seasonal fire-climate -- multivariate boxplots -----
give.n <- function(x){
  return(c(y = -4, label = length(x)))
}

temp<-subset(fireClim,
             groupVeg %in% c("Shrubland","Conifer","Grassland")
             & seq>=-4
             & fireSeas %in% c("AMJ")
             & fireSizeClass2 %in% c("G+","F"))
temp<-temp[,c("groupVeg","seq","fireSeas","fireYear","fireSizeClass2","spi3","spei3","vpdmaxZ","tmaxZ","tdmeanZ")]
temp<- gather(temp, var, value, spi3:tdmeanZ, factor_key=TRUE)

ggplot(data=subset(temp,var %in% c("spi3","vpdmaxZ")),
           aes(as.factor(seq),value,fill=var))+
  #geom_line(alpha=0.5)+
  geom_boxplot(varwidth = TRUE, outlier.colour = NA)+
  stat_summary(fun.data = give.n, geom = "text")+
  geom_hline(yintercept = 0)+
  facet_grid(fireSizeClass2~groupVeg)+
  #ylim(-4,4)+
  ggtitle(paste0("composite antecedent seasonal climate - AMJ"))

# yearly plots
ecoregSF <- sf::st_as_sf(ecoreg)
yr=2020
seasX<-c("JAS")
temp<-subset(fireClim,
       groupVeg %in% c("Shrubland","Conifer","Grassland")
       & seq>=-4
       & fireSeas %in% seasX
       & fireYear==yr
       & fireSizeClass2 %in% c("G+","F"))
#temp<-temp[,c("groupVeg","seq","fireSeas","fireSizeClass2","fireYear","spi3","spei3","vpdmaxZ","tmeanZ")]
#temp<- gather(temp, var, value, spi3:tmeanZ, factor_key=TRUE)
temp<-temp[,c("groupVeg","seq","fireSeas","fireYear","fireSizeClass2","spi3","spei3","vpdmaxZ","tmaxZ","tdmeanZ")]
temp<- gather(temp, var, value, spi3:tdmeanZ, factor_key=TRUE)

p1<-ggplot(data=subset(temp,var %in% c("spi3","vpdmaxZ","tmaxZ","tdmeanZ")),
       aes(as.factor(seq),value,fill=var))+
  #geom_line(alpha=0.5)+
  geom_boxplot(varwidth = TRUE, outlier.colour = "black")+
  #stat_summary(fun.data = give.n, geom = "text")+
  geom_hline(yintercept = 0)+
  facet_grid(fireSeas~groupVeg)+
  stat_summary(fun.data = give.n, geom = "text")+
  ylim(-4.5,4.5)+
  ggtitle(paste0("composite antecedent seasonal climate - ",yr))

p2<-ggplot()+
  geom_sf(data=ecoregSF, aes(), color='grey75', linewidth=0.5, alpha=1)+
  geom_sf(data=subset(perimsCrop,
                      GROUPVEG %in% c("Conifer","Shrubland","Grassland") &
                      year==yr &
                      seas %in% seasX),
          aes(fill=GROUPVEG), color='black', linewidth=0.25, alpha=1)+
  coord_sf()+
  ggtitle(paste0("MTBS Fire Locations - ",yr))

plot_grid(p1, p2, align = "v", ncol=1, axis="t", rel_heights = c(2,1))
#####

##### V2 antecedent seasonal fire-climate -- multivariate boxplots -----
give.n <- function(x){
  return(c(y = -4, label = length(x)))
}

temp<-subset(fireClim,
             groupVeg %in% c("Shrubland","Conifer","Grassland")
             & seq>=-4
             & fireSeas %in% c("AMJ")
             & fireSizeClass2 %in% c("G+","F"))
temp<-temp[,c("groupVeg","seq","fireSeas","fireYear","fireSizeClass2","spi3","spei3","vpdmaxZ","tmaxZ","tdmeanZ")]
temp<- gather(temp, var, value, spi3:tdmeanZ, factor_key=TRUE)

#ggplot(data=subset(temp,var %in% c("spi3","vpdmaxZ","tmaxZ","tdmeanZ")),
ggplot(data=subset(temp,var %in% c("spi3","vpdmaxZ")),
       aes(as.factor(seq),value,fill=groupVeg))+
  #geom_line(alpha=0.5)+
  geom_boxplot(varwidth = TRUE, outlier.colour = NA)+
  #geom_violin()+
  #stat_summary(fun.y=median, geom="point", size=2, color="red")+
  #geom_boxplot(width=0.1)+
  stat_summary(fun.data = give.n, geom = "text")+
  geom_hline(yintercept = 0)+
  facet_grid(var~fireSizeClass2)+
  #ylim(-4,4)+
  ggtitle(paste0("composite antecedent seasonal climate - AMJ"))

# violin plot
dodge <- position_dodge(width = 0.75)
ggplot(data=subset(temp,var %in% c("spi3","vpdmaxZ")),
       aes(as.factor(seq),value,fill=fireSizeClass2))+
  geom_boxplot(varwidth = TRUE, outlier.colour = NA)+
  #geom_violin(position = dodge)+
  #geom_boxplot(width=.1, outlier.colour=NA, position = dodge)+ 
  #stat_summary(fun.data = give.n, geom = "text")+
  geom_hline(yintercept = 0)+
  geom_hline(yintercept=1, linetype='dotted', col = 'black')+
  geom_hline(yintercept=-1, linetype='dotted', col = 'black')+
  facet_grid(var~groupVeg)+
  #ylim(-4,4)+
  ggtitle(paste0("composite antecedent seasonal climate - AMJ"))



# yearly plots
ecoregSF <- sf::st_as_sf(ecoreg)
yr=2011
seasX<-c("AMJ")
temp<-subset(fireClim,
             groupVeg %in% c("Shrubland","Conifer","Grassland")
             & seq>=-4
             & fireSeas %in% seasX
             & fireYear==yr
             & fireSizeClass2 %in% c("G+","F"))
#temp<-temp[,c("groupVeg","seq","fireSeas","fireSizeClass2","fireYear","spi3","spei3","vpdmaxZ","tmeanZ")]
#temp<- gather(temp, var, value, spi3:tmeanZ, factor_key=TRUE)
temp<-temp[,c("groupVeg","seq","fireSeas","fireYear","fireSizeClass2","spi3","spei3","vpdmaxZ","tmaxZ","tdmeanZ")]
temp<- gather(temp, var, value, spi3:tdmeanZ, factor_key=TRUE)

p1<-ggplot(data=subset(temp,var %in% c("spi3","vpdmaxZ","tmaxZ","tdmeanZ")),
           aes(as.factor(seq),value,fill=groupVeg))+
  #geom_line(alpha=0.5)+
  geom_boxplot(varwidth = TRUE, outlier.colour = "black")+
  #stat_summary(fun.data = give.n, geom = "text")+
  geom_hline(yintercept = 0)+
  #facet_grid(var~fireSizeClass2)+
  facet_grid(var~.)+
  #stat_summary(fun.data = give.n, geom = "text")+
  ylim(-4.5,4.5)+
  ggtitle(paste0("AMJ - composite antecedent seasonal climate - ",yr))

p2<-ggplot()+
  geom_sf(data=ecoregSF, aes(), color='grey75', linewidth=0.5, alpha=1)+
  geom_sf(data=subset(perimsCrop,
                      GROUPVEG %in% c("Conifer","Shrubland","Grassland") &
                        year==yr &
                        seas %in% seasX),
          aes(fill=GROUPVEG), color='black', linewidth=0.25, alpha=1)+
  coord_sf()+
  ggtitle(paste0("MTBS Fire Locations - ",yr))

plot_grid(p1, p2, align = "v", ncol=1, axis="t", rel_heights = c(1,1))
#####

#####
## model seasonal VPD max 
temp<-subset(fireClim,
             groupVeg %in% c("Shrubland","Conifer","Grassland")
             & seq>=0
             & fireSeas %in% c("AMJ")
             & fireSizeClass2 %in% c("G+","F"))
temp<-temp[,c("groupVeg","seq","fireSeas","fireYear","fireSize","fireSizeClass2","spi3","spei3","vpdmaxZ","tmaxZ","tdmeanZ")]
temp<-na.omit(temp)

# single model
m<-lm(vpdmaxZ ~ tmaxZ + tdmeanZ, data=temp)
  summary(m)
temp$resid = resid(m)

ggplot(temp, aes(fireYear, tdmeanZ, color=fireSizeClass2))+
  geom_point()

#temp<-temp[,c("groupVeg","seq","fireSeas","fireYear","fireSizeClass2","spi3","spei3","vpdmaxZ","tmaxZ","tdmeanZ","resid")]
temp<- gather(temp, var, value, spi3:resid, factor_key=TRUE)
#temp<- subset(temp, var %in% c("spi3","vpdmaxZ","tmaxZ","tdmeanZ","resid"))

ggplot(subset(temp, var %in% c("spi3","vpdmaxZ","tmaxZ","tdmeanZ")),
       aes(fireYear, value, group=fireYear))+
  geom_boxplot()+
  geom_hline(yintercept = 0)+
  facet_grid(var~.)+
  ggtitle("AMJ Fire Events - Climate Variables")

ggplot(subset(temp, var %in% c("vpdmaxZ","tdmeanZ","tmaxZ","resid")),
       aes(fireYear, value, group=fireYear))+
  geom_point()+
  geom_hline(yintercept = 0)+
  facet_grid(var~.)

# median by year
yearStat <- temp %>% group_by(fireYear,var) %>%
                      summarise(median=median(value),
                                n=n())
p1<-ggplot(subset(yearStat, var %in% c("spi3")), aes(fireYear,median, color=var))+
  geom_line()+
  #geom_point(aes(fireYear,median, size=n))+
  geom_hline(yintercept = 0)+
  ggtitle("AMJ Fire Events - Median SPI-3 Values")
  #geom_smooth(method="lm")
p2<-ggplot(subset(yearStat, var %in% c("vpdmaxZ","tmaxZ","tdmeanZ")), aes(fireYear,median, color=var))+
  geom_line()+
  #geom_point(aes(fireYear,median, size=n))+
  geom_hline(yintercept = 0)+
  ggtitle("AMJ Fire Events - Median Aridity Values")

plot_grid(p1, p2, align = "v", ncol=1, axis="t", rel_heights = c(1,1))

p2<-ggplot(subset(yearStat, var %in% c("spi3","vpdmaxZ","tmaxZ","tdmeanZ")), aes(fireYear,median, color=var))+
  geom_line()+
  #geom_point(aes(fireYear,median, size=n))+
  geom_hline(yintercept = 0)+
  ggtitle("AMJ Fire Events - Median Aridity Values")


#####
# by group
# varModel<- temp %>% group_by(groupVeg) %>%
#   do(broom::tidy(lm(vpdmaxZ ~ tmaxZ + tdmeanZ, .)))
# varModelR2<- temp %>% group_by(groupVeg) %>%
#   do(broom::glance(lm(vpdmaxZ ~ tmaxZ + tdmeanZ, .)))
#####




#####





#####


# test<-fireClim %>% group_by(seq,groupVeg, fireSeas) %>%
#   summarize(spi3=mean(spi3, na.rm=TRUE),
#             n=n())
# 
# ggplot(test, aes(seq, spi3, color=groupVeg, group=groupVeg))+
#   geom_line()+
#   facet_wrap(.~fireSeas)+
#   geom_hline(yintercept = 0)
# 
# test<-test[,c("seas","seq","spi3","meanSPI3","sdSPI3")]
# test<-gather(test, var, value, spi3:meanSPI3)
# test$sdSPEI3<-ifelse(test$var=="spi3",0,test$sdSPI3)
# ggplot(data = test, aes(x = seq, group = var)) + 
#   geom_line(aes(y = value, color = var), linewidth = 1) + 
#   geom_ribbon(aes(y = value, ymin = value - sdSPI3, ymax = value + sdSPI3, fill = var), alpha = .2)

#which(mtbsDF$Incid_Name=="MESCAL")

##### classification for antecedent seasonal sequences -----
temp<-fireClim[,c("eco1","fireSeas","fireSize","fireName",
                  "fireYear","ecoName","groupVeg","fireSizeClass",
                  "fireSizeClass2","seq","spi3")]

temp <- temp %>% 
        pivot_wider(names_from = seq, values_from = spi3)

temp1<-temp[,c(-1,-3,-4,-5,-6,-8)]
temp1<-subset(temp1, fireSeas=="AMJ" & groupVeg=="Shrubland")
temp1<-temp1[,c(-1,-2)]
colnames(temp1)[2:10]<-paste0("seq_",abs(as.numeric(colnames(temp1)[2:10])))

# try out classification on fire size 
# http://www.sthda.com/english/articles/35-statistical-machine-learning-essentials/141-cart-model-decision-tree-essentials/
library(rpart)
library(caret)
model <- rpart(fireSizeClass2 ~., data = temp1)
par(xpd = NA) # otherwise on some devices the text is clipped
plot(model)
text(model, digits = 3)

# create training datasets
temp1 <- na.omit(temp1)
# Inspect the data
sample_n(temp1, 3)
# Split the data into training and test set
training.samples <- temp1$fireSizeClass2 %>% 
  caret::createDataPartition(p = 0.8, list = FALSE)
train.data  <- temp1[training.samples, ]
test.data <- temp1[-training.samples, ]

# Build the model
set.seed(123)
model1 <- rpart(fireSizeClass2 ~., data = train.data, method = "class")
# Plot the trees
par(xpd = NA) # Avoid clipping the text in some device
plot(model1)
text(model1, digits = 3)

predicted.classes <- model1 %>% 
  predict(test.data, type = "class")
head(predicted.classes)

# Compute model accuracy rate on test data
mean(predicted.classes == test.data$fireSizeClass2)

# Fit the model on the training set

set.seed(123)
model2 <- caret::train(
  fireSizeClass2 ~., data = train.data, method = "rpart",
  trControl = caret::trainControl("cv", number = 10),
  tuneLength = 10
)
# Plot model accuracy vs different values of
# cp (complexity parameter)
plot(model2)

# Print the best tuning parameter cp that
# maximizes the model accuracy
model2$bestTune

# Plot the final tree model
par(xpd = NA) # Avoid clipping the text in some device
plot(model2$finalModel)
text(model2$finalModel,  digits = 3)

library(party)
set.seed(123)
model <- train(
  fireSizeClass2 ~., data = train.data, method = "ctree2",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(maxdepth = 6, mincriterion = 0.95 )
)
plot(model$finalModel)
######

##### kmeans clustering for antecedent seasonal sequences ----
# https://www.datanovia.com/en/blog/types-of-clustering-methods-overview-and-quick-start-r-code/
temp<-fireClim[,c("eco1","fireSeas","fireSize","fireName",
                  "fireYear","ecoName","groupVeg","fireSizeClass",
                  "fireSizeClass2","seq","spi3")]

temp<-subset(temp, fireSeas=="JAS" & seq>=(-8))

temp <- temp %>% 
  pivot_wider(names_from = seq, values_from = spi3)
temp <- na.omit(temp)
temp1<-temp[,c(10:ncol(temp))]

colnames(temp1)[1:ncol(temp1)]<-paste0("seq_",abs(as.numeric(colnames(temp1)[1:ncol(temp1)])))

# calc/viz clusters
factoextra::fviz_nbclust(temp1, kmeans, method = "gap_stat")

set.seed(123)
km.res <- kmeans(temp1, 2, nstart = 25)
# Visualize
library("factoextra")
fviz_cluster(km.res, data = temp1,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())
temp$cluster<-km.res$cluster
t1<-table(temp$groupVeg, temp$cluster, temp$fireSizeClass2)
  prop.table(t1, margin=1)
t1<-table(temp$fireSizeClass, temp$cluster)
  prop.table(t1)
t1<-table(temp$fireYear, temp$cluster)
  
  
# plot kmeans
kmns_centers<-as.data.frame(km.res$centers)
kmns_centers$cluster<-1:nrow(kmns_centers)
kmns_centers<-gather(kmns_centers, var, val, seq_8:seq_0, factor_key = TRUE)

ggplot(kmns_centers, aes(var, val, color=as.factor(cluster), group=as.factor(cluster)))+
  geom_line()+
  geom_hline(yintercept = 0)+
  ggtitle("JAS SPI-3 Kmeans clusters")

ggplot(temp, aes(fireYear,fill=factor(cluster)))+
  geom_bar(stat="count")+
  ggtitle("JAS SPI-3 Kmeans cluster yrly counts")

# Compute PAM
#library("cluster")
pam.res <- cluster::pam(temp1, 3)
# Visualize
fviz_cluster(pam.res)
temp$cluster<-pam.res$cluster
table(temp$groupVeg, temp$cluster)

# hopkins stats
gradient.color <- list(low = "steelblue",  high = "white")
temp1 %>%    # Remove column 5 (Species)
  #scale() %>%     # Scale variables
  get_clust_tendency(n = 50, gradient = gradient.color)

# nbclust 
#library("NbClust")
res.nbclust <- temp1 %>%
  #scale() %>%
  NbClust::NbClust(distance = "euclidean",
          min.nc = 2, max.nc = 15, 
          method = "complete", index ="all") 
# Visualize
#library(factoextra)
factoextra::fviz_nbclust(res.nbclust, ggtheme = theme_minimal())

set.seed(123)
km.res <- kmeans(temp1, 2, nstart = 25)
# Visualize
library("factoextra")
fviz_cluster(km.res, data = temp1,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

#####



