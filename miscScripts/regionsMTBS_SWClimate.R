# SW Wildfire analysis - using MTBS perimeters , other regions
# MAC 11/28/23
# adapted from burn period research fodAnalysis.R, FOD_SWClimate.R, regionsFOD_SWClimate.R
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

##### extract ecoRegion climate time series -----
# 
# library(SPEI)
# library(tidyverse)
# 
# load("~/RProjects/SWWildfires/data/mtbsSW_ecoreg.RData")
# 
# # combine ecoregions
# ecoregSW <- raster::aggregate(ecoreg, 'US_L3NAME')
#  #sp::spplot(ecoregSW, "US_L3NAME")
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
# ecoClim<-list()
# for(j in 1:nrow(ecoregSW)){
# 
#   print(j)
# 
#   climTS<-list()
#   for(i in 1:nrow(climFiles)){
#     # temp raster var
#     #var<-stack(climFiles$path[i])
#     var<-terra::rast(climFiles$path[i])
#     # extract time series
#     ext<-as.data.frame(t(terra::extract(var, terra::vect(ecoregSW[j,]), ID=FALSE, raw=FALSE, fun=mean)))
#     ext<-as.data.frame(ext[2:nrow(ext),])
#     colnames(ext)<-climFiles$var[i]
#     climTS[[i]]<-ext
#     #print(climFiles$var[i])
#   }
#   # create fire dataframe
#   climTS<-do.call(cbind, climTS)
#   climTS<-cbind.data.frame(ecoregSW@data$US_L3NAME[j],
#                            dates,climTS)
#   colnames(climTS)[1]<-c("eco1")
#   # calculate SPI
#   climTS$spi3<-spi(climTS$prec,3, na.rm = TRUE)$fitted
#   climTS$spi6<-spi(climTS$prec,6, na.rm = TRUE)$fitted
#   climTS$spi12<-spi(climTS$prec,12, na.rm = TRUE)$fitted
#   climTS$spi24<-spi(climTS$prec,24, na.rm = TRUE)$fitted
#   # calculate SPEI
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
#   # 3-month anoms, swap in rolling avg/sum
#   anoms<-cbind.data.frame(climTS$dates,climTS$month,ecoregSW@data$US_L3NAME[j],clim3mo)
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
#   # fixed 3-mo seasons
#   anoms$seas<-cut(anoms$month,c(0,3,6,9,12))
#   levels(anoms$seas) = c("JFM","AMJ","JAS","OND")
#   anoms<-subset(anoms, month %in% c(3,6,9,12))
# 
#   # write to list
#   ecoClim[[j]]<-anoms
# 
#   # save full anoms
#   #climAnom[[j]]<-anoms
# 
# }
# tictoc::toc()
# 
# save(ecoClim, file="./data/AZNM_ecoregion_climate_3moRoll.RData")
#####


##### extract ecoRegion GridMet fire weather vars time series ----
# 
# library(tidyverse)
# 
# load("~/RProjects/SWWildfires/data/mtbsSW_ecoreg.RData")
# 
# # # combine ecoregions
# ecoregSW <- raster::aggregate(ecoreg, 'US_L3NAME')
# #  #sp::spplot(ecoregSW, "US_L3NAME")
# 
# # update scratch dir with PRISM data 1895-2022 monthly precip/mean temp
# # update gridmet data using ~/FireClimate/monsoonClimo/processGridMet.R
# climFiles<-as.data.frame(list.files(path = "/scratch/crimmins/gridmet/update_Aug2019/processed/SW_PSA", pattern="*.grd",full.names = TRUE, recursive = TRUE))
# climFiles$fileName<-(list.files(path = "/scratch/crimmins/gridmet/update_Aug2019/processed/SW_PSA", pattern="*.grd",full.names = FALSE, recursive = TRUE))
# colnames(climFiles)<-c("path","name")
# climFiles<-climFiles %>% 
#   separate(name, sep="_", into=c("region","PSA","dataset","timestep","var", "being","end"))
# climFiles<-subset(climFiles, end=="2022.grd")
# climFiles$path<-as.character(climFiles$path)
# climFiles<-climFiles[1:4,]
# ###
# 
# ###
# # dates 1979-2022 GridMet data
# dates=seq(as.Date("1979-01-01"), as.Date("2022-12-31"), by="day")
# dates<-as.data.frame(dates)
# dates$month<-as.numeric(format(dates$dates, "%m"))
# dates$year<-as.numeric(format(dates$dates, "%Y"))
# dates$doy<-as.numeric(format(dates$dates,"%j"))
# length(unique(dates$year))
# 
# # extract time series from fire perimeter
# ecoClim<-list()
# 
# for(j in 1:length(ecoregSW)){
#   
#   print(j)
#   
#     climTS<-list()
#     for(i in 1:nrow(climFiles)){
#       # temp raster var
#       #var<-stack(climFiles$path[i])
#       print(climFiles$var[i])
#       var<-terra::rast(climFiles$path[i])
#       # extract time series
#       #ext1<-as.data.frame(t(terra::extract(var, terra::vect(ecoregSW[j,]), ID=FALSE, raw=FALSE, fun=mean)))
#       #ext1<-as.data.frame(ext1[2:nrow(ext1),])
#       ext<-as.data.frame(t(exactextractr::exact_extract(var,ecoregSW[j,],'mean')))
#       ext<-as.data.frame(ext[1:nrow(ext),])
#       colnames(ext)<-climFiles$var[i]
#       climTS[[i]]<-ext
#       #print(climFiles$var[i])
#     }
#   # create fire dataframe
#   climTS<-do.call(cbind, climTS)
#   climTS<-cbind.data.frame(ecoregSW@data$US_L3NAME[j],dates,climTS)
#   colnames(climTS)[1]<-"eco1"
#   
#   # calculate monthly averages
#   climTS<-climTS %>% group_by(eco1,month,year) %>%
#                 summarise(bi=mean(bi, na.rm=TRUE),
#                           erc=mean(erc, na.rm=TRUE),
#                           fm100=mean(fm100, na.rm=TRUE),
#                           fm1000=mean(fm1000, na.rm=TRUE))
#   climTS$dates<-as.Date(paste0(climTS$year,"-",climTS$month,"-01"))
#   climTS <- climTS[order(climTS$dates),]
#   
#   # 3-month moving sums/avgs
#   clim3mo<-cbind.data.frame(zoo::rollapply(climTS[,c("bi","erc","fm100","fm1000")], FUN = mean, width = 3,
#                                            fill=NA,align="right", by.column = TRUE))
#   #colnames(clim3mo)<-paste0(colnames(clim3mo),"3mo")
# 
#   # 3-month anoms, swap in rolling avg/sum
#   anoms<-cbind.data.frame(climTS$dates,climTS$month,ecoregSW@data$US_L3NAME[j],clim3mo)
#   colnames(anoms)[1:3]<-c("dates","month","eco1")
#   # calculate anomalies
#   #anoms<-climTS[,6:14]
#   meanMO<- anoms %>% group_by(month) %>%
#     summarise(meanBI=mean(bi, na.rm=TRUE),
#               meanERC=mean(erc, na.rm=TRUE),
#               meanFM100=mean(fm100, na.rm=TRUE),
#               meanFM1000=mean(fm1000, na.rm=TRUE))
#   anoms<-merge(anoms,meanMO, by=c("month"))
#   # calculate anoms
#   anoms$anomBI<-anoms$bi-anoms$meanBI
#   anoms$anomERC<-anoms$erc-anoms$meanERC
#   anoms$anomFM100<-anoms$fm100-anoms$meanFM100
#   anoms$anomFM1000<-anoms$fm1000-anoms$meanFM1000
#   anoms <- anoms[order(anoms$dates),]
# 
#   # fixed 3-mo seasons
#   anoms$seas<-cut(anoms$month,c(0,3,6,9,12))
#   levels(anoms$seas) = c("JFM","AMJ","JAS","OND")
#   anoms<-subset(anoms, month %in% c(3,6,9,12))
# 
#   # write to list
#   ecoClim[[j]]<-anoms
#   # save full anoms
#   #climAnom[[j]]<-anoms
# }
# 
# save(ecoClim, file="./data/AZNM_ecoregion_gridmetFire_3moRoll.RData")
# 
# #####


##### analyze fires and ecoregion climate ----
library(tidyverse)
library(ggplot2)
library(cowplot)

load("~/RProjects/SWWildfires/data/mtbsSW_ecoreg.RData")
# load seas fire weather
load("./data/AZNM_ecoregion_gridmetFire_3moRoll.RData")
ecoWx<-ecoClim
rm(ecoClim)
# load seas climate
load("./data/AZNM_ecoregion_climate_3moRoll.RData")

# filter out border state fires based on event ID
perimsCrop$event_st<-substr(perimsCrop$Event_ID,1,2)
perimsCrop<-subset(perimsCrop, event_st %in% c("AZ","NM"))

# drop fires without groupVeg
#sum(perimsCrop$BurnBndAc[perimsCrop$GROUPVEG=="Fill-Not Mapped"])/sum(perimsCrop$BurnBndAc)
perimsCrop<-subset(perimsCrop, !(GROUPVEG %in% c("Fill-Not Mapped")))

# create DF
mtbsDF<-sf::st_drop_geometry(perimsCrop)
# droplevels
mtbsDF<-droplevels(mtbsDF)
mtbsDF$month<-as.numeric(format(mtbsDF$Ig_Date,"%m"))
mtbsDF$seas<-cut(mtbsDF$month,c(0,3,6,9,12))
levels(mtbsDF$seas) = c("JFM","AMJ","JAS","OND")

##### summary stats ----
# map of ecoreg
ecoregSF <- sf::st_as_sf(ecoreg)
ggplot()+
  geom_sf(data=ecoregSF, aes(), color='grey75', linewidth=0.5, alpha=1)+
  geom_sf(data=(perimsCrop), aes(fill=as.factor(GROUPVEG)), color='black', linewidth=0.25, alpha=1)+
  coord_sf()+
  ggtitle("MTBS Fire Locations, 1984-2021")
#plotly::ggplotly(p)

# veg group proportions
temp<-mtbsDF %>%
  group_by(GROUPVEG,US_L3NAME) %>%
  summarise(nFires=n(),
            totAc=sum(BurnBndAc))
temp$ratio<-temp$totAc/temp$nFires
ggplot(temp, aes(x=US_L3NAME, y=totAc, fill=GROUPVEG))+
  geom_bar(width = 0.75, stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# fire event months
ggplot(mtbsDF, aes(month, color=US_L3NAME))+
  #geom_histogram(bins=12, position="dodge")+
  geom_freqpoly(binwidth = 1)+
  ggtitle("Fire Counts/month")

# create summary table time series
temp<-mtbsDF %>%
  group_by(year, GROUPVEG) %>%
  summarise(nFires=n(),
            totAc=sum(BurnBndAc))
temp$ratio<-temp$totAc/temp$nFires
temp<- gather(temp, var, value, nFires:ratio, factor_key=TRUE)

ggplot(temp, aes(year,value,fill=GROUPVEG))+
  geom_bar(position="stack", stat="identity")+
  facet_wrap(.~var, ncol=1, scales="free")

ggplot(subset(temp, var=="ratio"), aes(year,value,fill=GROUPVEG))+
  geom_bar(position="stack", stat="identity")+
  #facet_wrap(.~var, ncol=1, scales="free")
  facet_grid(GROUPVEG~var)

temp<-mtbsDF %>%
  group_by(year, US_L3NAME) %>%
  summarise(nFires=n(),
            totAc=sum(BurnBndAc))
temp$ratio<-temp$totAc/temp$nFires
temp<- gather(temp, var, value, nFires:ratio, factor_key=TRUE)

ggplot(subset(temp,var=="ratio"), aes(year,value, fill=US_L3NAME))+
  geom_bar(position="stack", stat="identity")+
  #facet_grid(US_L3NAME~var, scales="free")
  facet_grid(US_L3NAME~var)

# ecoregion climograph
ggplot(seasFire, aes(seas,prec))+
  geom_boxplot()+
  facet_wrap(.~US_L3NAME)

#####

##### EcoRegion Fire-Climate ts -----

# prep ecoClim
ecoClim<-do.call(rbind, ecoClim)
ecoClim$year<-as.numeric(format(ecoClim$dates, "%Y"))
ecoClim<-droplevels(ecoClim)

# prep ecoWx
ecoWx<-do.call(rbind, ecoWx)
ecoWx$year<-as.numeric(format(ecoWx$dates, "%Y"))
ecoWx<-droplevels(ecoWx)
# drop duplicated cols
ecoWx<-ecoWx[,-which(names(ecoWx) %in% c("month","seas","year"))]

# join wx to clim
ecoClim<-merge(ecoClim,ecoWx,by=c("dates","eco1"))

##### seasonal fire-climate plots -------
seasFire<-mtbsDF %>%
  group_by(US_L3NAME,seas,year) %>%
  summarise(nFires=n(),
            totAc=sum(BurnBndAc))
seasFire$ratio<-seasFire$totAc/seasFire$nFires

# join ecoClim
seasFire<-merge(seasFire,ecoClim,
                by.x=c("US_L3NAME","seas","year"),
                by.y=c("eco1","seas","year"), all.y=TRUE)
seasFire<-subset(seasFire, year>=1984 & year<=2021)
# join ecoWx
seasFire<-merge(seasFire,ecoWx,
                by.x=c("US_L3NAME","seas","year"),
                by.y=c("eco1","seas","year"), all.x=TRUE)

# fill in NA's with 0's
#temp$nFires[is.na(temp$nFires)] <- 0
#temp$totAc[is.na(temp$totAc)] <- 0

p1<-ggplot(subset(seasFire, US_L3NAME=="Arizona/New Mexico Mountains"),
       aes(dates,totAc, fill=seas))+
  geom_bar(position="stack", stat="identity")+
  facet_wrap(.~US_L3NAME)

p2<-ggplot(subset(seasFire, US_L3NAME=="Arizona/New Mexico Mountains"),
       aes(dates,anomTmean))+
    geom_line()+
    geom_hline(yintercept = 0)+
    facet_wrap(.~US_L3NAME)
plot_grid( p1, p2, ncol = 1, align='v', axis='blr')

ggplot(seasFire, aes(spi3,anomBI,color=seas))+
  geom_point()+
  geom_vline(xintercept = 0)+
  geom_smooth(method=lm)+
  facet_wrap(.~US_L3NAME, scales='free')

cor(seasFire$spi3,seasFire$anomBI)

#####

##### lagged climate analysis -----
# join summary table with antecedent climate
# https://www.r-bloggers.com/2022/07/how-to-calculate-lag-by-group-in-r/

# ecoreg~seas lagged correlations ----
ecoregSeas<-mtbsDF %>%
  group_by(seas, year, US_L3NAME) %>%
  summarise(nFires=n(),
            totAc=sum(BurnBndAc))

ggplot(ecoregSeas,aes(seas,nFires,fill=US_L3NAME))+
  geom_bar(stat="identity", position="stack")

tempClim<-ecoClim[,c("eco1","month","year","dates","seas","anomFM100")]
colnames(tempClim)[ncol(tempClim)]<-"lag_0"

tempClim<- tempClim %>%
  group_by(eco1) %>%
  mutate(lag_1= lag(lag_0, n=1, order_by=dates),
         lag_2= lag(lag_0, n=2, order_by=dates),
         lag_3= lag(lag_0, n=3, order_by=dates),
         lag_4= lag(lag_0, n=4, order_by=dates),
         lag_5= lag(lag_0, n=5, order_by=dates),
         lag_6= lag(lag_0))

#tempClim<- merge(tempClim,ecoregSeas,by.x=c("seas","year","eco1"),by.y=c("seas","year","US_L3NAME"))
tempClim<- merge(tempClim,ecoregSeas,by.x=c("seas","year","eco1"),
                 by.y=c("seas","year","US_L3NAME"),
                 all.x = TRUE)
tempClim<-subset(tempClim, year>=1984 & year<=2021)

# wide to long
temp<- gather(tempClim, lag, value, lag_0:lag_6, factor_key=TRUE)
# fill in NA's with 0's
temp$nFires[is.na(temp$nFires)] <- 0
temp$totAc[is.na(temp$totAc)] <- 0

# set lag var order
levels(temp) = c("lag_6","lag_5","lag_4","lag_3","lag_2","lag_1","lag_0")

# lagged correlations
corrs<- temp %>% group_by(seas,eco1, lag) %>%
        summarize(n=n(),
                  non0yrs=sum(totAc!=0),
                  cor=cor.test((totAc),value, method="spearman")$estimate,
                  pval=cor.test((totAc),value,method="spearman")$p.value,
                  sig = ifelse(pval < 0.05, "*", ""),
                  sigPos=max(cor))
summary(corrs$cor)
summary(corrs$cor[corrs$lag=="lag_0"])

# lag corr plot
ggplot(corrs, aes(lag,cor))+
  geom_bar(position="stack", stat="identity")+
  facet_grid(seas~eco1)+
  ggtitle("Antecedent Seasonal 3-mo SPI v nFires (Spearman Rank rho)")+
  geom_text(aes(label = sig, y = sigPos), size = 6, color="red",
            data = corrs)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#####  

# ecoreg~seas~groupveg lagged correlations ----
ecoregSeasVeg<-mtbsDF %>%
  group_by(seas, year, US_L3NAME, GROUPVEG) %>%
  summarise(nFires=n(),
            totAc=sum(BurnBndAc)) %>%
  ungroup() %>%
  complete(seas, year, US_L3NAME, GROUPVEG,
           fill = list(nFires = 0,
                       totAc = 0))

ggplot(ecoregSeasVeg,aes(seas,nFires,fill=GROUPVEG))+
  geom_bar(stat="identity", position="stack")+
  facet_grid(GROUPVEG~US_L3NAME)

tempClim<-ecoClim[,c("eco1","month","year","dates","seas","anomBI")]
colnames(tempClim)[ncol(tempClim)]<-"lag_0"

tempClim<- tempClim %>%
  group_by(eco1) %>%
  mutate(lag_1= lag(lag_0, n=1, order_by=dates),
         lag_2= lag(lag_0, n=2, order_by=dates),
         lag_3= lag(lag_0, n=3, order_by=dates),
         lag_4= lag(lag_0, n=4, order_by=dates),
         lag_5= lag(lag_0, n=5, order_by=dates),
         lag_6= lag(lag_0))

#tempClim<- merge(tempClim,ecoregSeas,by.x=c("seas","year","eco1"),by.y=c("seas","year","US_L3NAME"))
tempClim<- merge(tempClim,ecoregSeasVeg,by.x=c("seas","year","eco1"),
                 by.y=c("seas","year","US_L3NAME"),
                 all.x = TRUE)
tempClim<-subset(tempClim, year>=1984 & year<=2021)

# wide to long
temp<- gather(tempClim, lag, value, lag_0:lag_6, factor_key=TRUE)
# fill in NA's with 0's
#temp$nFires[is.na(temp$nFires)] <- 0
#temp$totAc[is.na(temp$totAc)] <- 0

# set lag var order
levels(temp) = c("lag_6","lag_5","lag_4","lag_3","lag_2","lag_1","lag_0")

# lagged correlations
corrs<- temp %>% group_by(seas,eco1, lag, GROUPVEG) %>%
  summarize(n=n(),
            non0yrs=sum(totAc!=0),
            cor=cor.test(totAc,value, method="spearman")$estimate,
            pval=cor.test(totAc,value,method="spearman")$p.value,
            sig = ifelse(pval < 0.05, "*", ""),
            sigPos=max(cor))
summary(corrs$cor)
tapply(corrs$cor, corrs$GROUPVEG, summary)

# lag corr plot
tempCorr<-subset(corrs,GROUPVEG=="Conifer")
ggplot(tempCorr, aes(lag,cor))+
  geom_bar(position="dodge", stat="identity")+
  facet_grid(seas~eco1)+
  ggtitle("Antecedent Seasonal 3-mo SPI v Conifer totAc (Spearman Rank rho)")+
  geom_text(aes(label = sig, y = sigPos), size = 6, color="red",
            data = tempCorr)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# lag corr plot
ggplot(subset(corrs,seas %in% c("AMJ")), aes(lag,cor, fill=GROUPVEG))+
  geom_bar(position="dodge", stat="identity")+
  facet_grid(seas+GROUPVEG~eco1)+
  ggtitle("Antecedent Seasonal 3-mo SPI v nFires (Spearman Rank rho)")+
  #geom_text(aes(label = sig, y = sigPos), size = 6, color="red",
  #          data = tempCorr)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(subset(temp, lag=="lag_0" & seas=="AMJ"), aes(value,log(totAc),color=seas))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_grid(GROUPVEG~eco1, scales = "free")

#####  

##### plots of key fire years ----

# "Sonoran Basin and Range"
# "Arizona/New Mexico Mountains

stDate <-'2009-01-01'
endDate<-'2012-01-01'
region<-"Arizona/New Mexico Mountains"

p1<-ggplot(subset(seasFire, US_L3NAME==region),
           aes(dates,totAc, fill=seas))+
  geom_bar(position="stack", stat="identity")+
  facet_wrap(.~US_L3NAME)+
  scale_x_date(date_breaks = "1 year", 
               labels=scales::date_format("%Y"),
               limits = as.Date(c(stDate,endDate)))

p2<-ggplot(subset(seasFire, US_L3NAME==region),
           aes(dates,spi3))+
  geom_line()+
  geom_point(aes(color=seas))+
  geom_hline(yintercept = 0)+
  facet_wrap(.~US_L3NAME)+
  scale_x_date(date_breaks = "1 year", 
               labels=scales::date_format("%Y"),
               limits = as.Date(c(stDate,endDate)))

p3<-ggplot(subset(seasFire, US_L3NAME==region),
           aes(dates,anomBI))+
  geom_line()+
  geom_point(aes(color=seas))+
  geom_hline(yintercept = 0)+
  facet_wrap(.~US_L3NAME)+
  scale_x_date(date_breaks = "1 year", 
               labels=scales::date_format("%Y"),
               limits = as.Date(c(stDate,endDate)))

plot_grid( p1, p2, p3, ncol = 1, align='v', axis='blr')






