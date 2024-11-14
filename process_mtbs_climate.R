# SW Wildfire analysis - using MTBS perimeters for climate extracts, other regions
# MAC 12/5/23
# adapted from burn period research regionsMTBS_SWClimate.R, fodAnalysis.R, FOD_SWClimate.R, regionsFOD_SWClimate.R
# using data from https://www.mtbs.gov/direct-download

##### CREATE MTBS dataframe with ecoregions, vegtypes -----
library(tidyverse)
library(ggplot2)
#load/filter mtbs perims, mtbs_perims.R
 load("./data/AZNM_mtbs_perims_1984_2024.RData")

# remove factors -- from ./BurnPeriodResearch/fireProgression_loop.R
 perimsCrop$BurnBndAc<-as.numeric(as.character(perimsCrop$BurnBndAc))
 perimsCrop$Incid_Name<-as.character(perimsCrop$Incid_Name)
 perimsCrop$Ig_Date<-as.Date(perimsCrop$Ig_Date, "%Y-%m-%d")
 perimsCrop$year<-as.numeric(format(as.Date(perimsCrop$Ig_Date, format="%Y/%m/%d"),"%Y"))
 perimsCrop<-subset(perimsCrop, Incid_Type!="Prescribed Fire")

# map layers
#states <- raster::getData('GADM', country='United States', level=1)
 states <- geodata::gadm(country = "USA", level = 1, path = tempdir())
# aznm<-subset(states, NAME_1=="Arizona" | NAME_1=="New Mexico")
 aznm <- states[states$NAME_1 %in% c("Arizona", "New Mexico"), ]
 aznm <- as(aznm, "Spatial")
 # ecoreg<-rgdal::readOGR(dsn="./data/shapes", layer="us_eco_l3")
 ecoreg <- sf::st_read("./data/shapes/us_eco_l3.shp")
 ecoreg <- as(ecoreg, "Spatial")
  ecoreg <- sp::spTransform(ecoreg,raster::crs(states))
# intersect polys
ecoreg<-raster::intersect(ecoreg,aznm)

#  add in ecoreg to points
#  https://gis.stackexchange.com/questions/270537/joining-attributes-by-location-for-more-than-one-located-feature-using-r
ecoreg <- sp::spTransform(ecoreg,raster::crs(perimsCrop))

# # us sf to join
sf::sf_use_s2(FALSE)
 perimsCrop <- sf::st_join(sf::st_as_sf(perimsCrop), sf::st_as_sf(ecoreg), left=TRUE)
 perimsCrop<-perimsCrop[!duplicated(perimsCrop$Event_ID),]
 perimsCrop <- subset(perimsCrop, !is.na(US_L3CODE))

# drop some columns
perimsCrop <- perimsCrop[,c(1:4,6,8:11,23,25,40)]

##### GROUPVEG using BPS https://landfire.gov/bps.php
# read in landfire data, use Terra library
#bps<-raster("./data/landfire/LF2020_BPS_220_CONUS/LC20_BPS_220.tif")
bps<-terra::rast("./data/landfire/LF2020_BPS_220_CONUS/LC20_BPS_220.tif")
  bps<- terra::project(bps, "EPSG:4326", method = "near")
# extract BPS name and groupveg
perimsCrop$groupVeg<-exactextractr::exact_extract(bps,perimsCrop,'mode')
  #bpsMeta<- read.csv('https://landfire.gov/CSV/LF2020/LF20_BPS_220.csv')
  bpsMeta<- read.csv('https://landfire.gov/sites/default/files/CSV/LF2016/LF16_BPS.csv')
  perimsCrop<-merge(perimsCrop, bpsMeta[,c(1,6)], by.x="groupVeg",by.y="VALUE")
##### end bps

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

save(perimsCrop,ecoreg, aznm, file="./data/mtbsSW_ecoreg.RData")
#####


##### extract MTBS climate time series -----
# 
library(SPEI)
library(SCI)
library(tidyverse)
library(sf)

load("./data/mtbsSW_ecoreg.RData")

perimsCrop<-subset(perimsCrop, year<=2021)

#load PRISM data, adapted from ./WinterSummerPrecip/extractClusterTS.R
#dates 1895-2022 PRISM data
dates=seq(as.Date("1895-01-01"), as.Date("2022-12-31"), by="month")
dates<-as.data.frame(dates)
dates$month<-as.numeric(format(dates$dates, "%m"))
dates$year<-as.numeric(format(dates$dates, "%Y"))
length(unique(dates$year))

# update scratch dir with PRISM data 1895-2022 monthly precip/mean temp
# use ~/RProjects/PRISMDownload/monthyDownloadPRISM.R
# process to subset using ~/RProjects/WinterSummerPrecip/processPRISM.R
climFiles<-as.data.frame(list.files(path = "./data/PRISM", pattern="*.grd",full.names = TRUE, recursive = TRUE))
climFiles$fileName<-(list.files(path = "./data/PRISM", pattern="*.grd",full.names = FALSE, recursive = TRUE))
colnames(climFiles)<-c("path","name")
climFiles<-climFiles %>%
  separate(name, sep="_", into=c("dataset", "var", "being","end"))
climFiles<-subset(climFiles, end=="2022.grd")
climFiles$path<-as.character(climFiles$path)
###
# extract time series from fire perimeter
climAnom<-list()
tictoc::tic()
mtbsClim<-list()
for(j in 1:nrow(perimsCrop)){

  print(j)

  climTS<-list()
  for(i in 1:nrow(climFiles)){
    # temp raster var
    #var<-stack(climFiles$path[i])
    var<-terra::rast(climFiles$path[i])
    # extract time series
    ext<-as.data.frame(t(terra::extract(var, terra::vect(perimsCrop[j,]), ID=FALSE, raw=FALSE, fun=mean)))
    #ext<-as.data.frame(ext[2:nrow(ext),])
    colnames(ext)<-climFiles$var[i]
    climTS[[i]]<-ext
    #print(climFiles$var[i])
  }
  # create fire dataframe
  climTS<-do.call(cbind, climTS)
  climTS<-cbind.data.frame(perimsCrop$Event_ID[j],
                           dates,climTS)
  colnames(climTS)[1]<-c("eco1")

  # subset to different period of record
  climTS<-subset(climTS, year<=2021)

  # calculate SPI using SPEI package
  # climTS$spi3<-spi(climTS$prec,3, na.rm = TRUE)$fitted
  # climTS$spi6<-spi(climTS$prec,6, na.rm = TRUE)$fitted
  # climTS$spi12<-spi(climTS$prec,12, na.rm = TRUE)$fitted
  # climTS$spi24<-spi(climTS$prec,24, na.rm = TRUE)$fitted

  # calculate SPI with SCI package
  spi.para<-fitSCI(climTS$prec,first.mon=1,distr="gamma",time.scale=3,p0=TRUE)
    climTS$spi3<-transformSCI(climTS$prec,first.mon=1,obj=spi.para,sci.limit=4)
  spi.para<-fitSCI(climTS$prec,first.mon=1,distr="gamma",time.scale=6,p0=TRUE)
    climTS$spi6<-transformSCI(climTS$prec,first.mon=1,obj=spi.para,sci.limit=4)
  spi.para<-fitSCI(climTS$prec,first.mon=1,distr="gamma",time.scale=12,p0=TRUE)
    climTS$spi12<-transformSCI(climTS$prec,first.mon=1,obj=spi.para,sci.limit=4)
  spi.para<-fitSCI(climTS$prec,first.mon=1,distr="gamma",time.scale=24,p0=TRUE)
    climTS$spi24<-transformSCI(climTS$prec,first.mon=1,obj=spi.para,sci.limit=4)

  # calculate SPEI using SPEI package
  climTS$spei3<-spei(climTS$prec-climTS$hargreaves,3, na.rm=TRUE)$fitted
  climTS$spei6<-spei(climTS$prec-climTS$hargreaves,6, na.rm=TRUE)$fitted
  climTS$spei12<-spei(climTS$prec-climTS$hargreaves,12, na.rm=TRUE)$fitted
  climTS$spei24<-spei(climTS$prec-climTS$hargreaves,24, na.rm=TRUE)$fitted

  # 3-month moving sums/avgs
  clim3mo<-cbind.data.frame(zoo::rollapply(climTS[,c("hargreaves","prec")], FUN = sum, width = 3,
                                           fill=NA,align="right", by.column = TRUE),
                            zoo::rollapply(climTS[,c("tdmean","tmax","tmean","tmin","vpdmax")], FUN = mean, width = 3,
                                           fill=NA,align="right", by.column = TRUE))
  #colnames(clim3mo)<-paste0(colnames(clim3mo),"3mo")

  # calculate z-scores
  clim3moZ<-cbind.data.frame(climTS$dates,climTS$month,clim3mo)
  colnames(clim3moZ)[1:2]<-c("dates","month")
  clim3moZ<-clim3moZ %>%
    group_by(month) %>%
    mutate(tmeanZ = (tmean-mean(tmean, na.rm=TRUE))/sd(tmean, na.rm=TRUE),
           vpdmaxZ = (vpdmax-mean(vpdmax, na.rm=TRUE))/sd(vpdmax, na.rm=TRUE),
           hargZ = (hargreaves-mean(hargreaves, na.rm=TRUE))/sd(hargreaves, na.rm=TRUE),
           tdmeanZ = (tdmean-mean(tdmean, na.rm=TRUE))/sd(tdmean, na.rm=TRUE),
           tmaxZ = (tmax-mean(tmax, na.rm=TRUE))/sd(tmax, na.rm=TRUE),
           tminZ = (tmin-mean(tmin, na.rm=TRUE))/sd(tmin, na.rm=TRUE))
  clim3moZ<-clim3moZ[,c("dates","tmeanZ","vpdmaxZ","hargZ","tdmeanZ","tmaxZ","tminZ")]

  # 3-month anoms, swap in rolling avg/sum
  anoms<-cbind.data.frame(climTS$dates,climTS$month,perimsCrop$Event_ID[j],clim3mo)
  colnames(anoms)[1:3]<-c("dates","month","eco1")
  # calculate anomalies
  #anoms<-climTS[,6:14]
  meanMO<- anoms %>% group_by(month) %>%
    summarise(meanHargreaves=mean(hargreaves, na.rm=TRUE),
              meanPrec=mean(prec, na.rm=TRUE),
              meanTDmean=mean(tdmean, na.rm=TRUE),
              meanTmax=mean(tmax, na.rm=TRUE),
              meanTmean=mean(tmean, na.rm=TRUE),
              meanTmin=mean(tmin, na.rm=TRUE),
              meanVPDmax=mean(vpdmax, na.rm=TRUE))
  anoms<-merge(anoms,meanMO, by=c("month"))
  # calculate anoms
  anoms$anomHarg<-anoms$hargreaves-anoms$meanHargreaves
  anoms$anomPrec<-anoms$prec-anoms$meanPrec
  anoms$anomTDmean<-anoms$tdmean-anoms$meanTDmean
  anoms$anomTmax<-anoms$tmax-anoms$meanTmax
  anoms$anomTmean<-anoms$tmean-anoms$meanTmean
  anoms$anomTmin<-anoms$tmin-anoms$meanTmin
  anoms$anomVPDmax<-anoms$vpdmax-anoms$meanVPDmax
  anoms <- anoms[order(anoms$dates),]

  # add spi back in
  anoms<-merge(anoms,climTS[,c("dates","spi3","spi6","spi12","spi24",
                               "spei3","spei6","spei12","spei24")], by=c("dates"))

  # add z-scores back in
  anoms<-merge(anoms, clim3moZ, by="dates")

  # fixed 3-mo seasons
  anoms$seas<-cut(anoms$month,c(0,3,6,9,12))
  levels(anoms$seas) = c("JFM","AMJ","JAS","OND")
  anoms<-subset(anoms, month %in% c(3,6,9,12))

  # subset to recent decades
  anoms<-subset(anoms, dates>="1980-01-01")

  # write to list
  mtbsClim[[j]]<-anoms

  # save full anoms
  #climAnom[[j]]<-anoms

}
tictoc::toc()

save(mtbsClim, file="./data/AZNM_mtbs_fires_climate_3moRoll.RData")
####


##### create fireClim.Rds
##### analyze fires and mtbs fireperim climate ----
library(tidyverse)
library(ggplot2)
library(cowplot)

## function
rowSd <- function (x, na.rm=FALSE) apply(X=x, MARGIN=1, FUN=sd, na.rm=na.rm)
##

# data from process_mtbs_climate.R
load("./data/mtbsSW_ecoreg.RData")
load("./data/AZNM_mtbs_fires_climate_3moRoll.RData")

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

perimsCrop<-subset(perimsCrop, year<=2021)

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
fireClim<-list()
# gather antecedent climate for each fire
for(i in 1:nrow(mtbsDF)){

  tempClim<-mtbsClim[[i]]
  tempFire<-mtbsDF[i,]

  # assign fire month to fixed season
  fireMo<-as.numeric(format(mtbsDF$Ig_Date[i], "%m"))
  fireMo<-ifelse(fireMo==12 & fireMo>=10, 12,
                 ifelse(fireMo>=7 & fireMo<=9, 9,
                        ifelse(fireMo>=4 && fireMo<=6, 6,3)))
  fireDate<-as.Date(paste0(format(mtbsDF$Ig_Date[i], "%Y"),"-",fireMo,"-01"))
  # subset to fire event
  firePrior<-lubridate::ymd(fireDate)-lubridate::years(2)
  temp<-subset(tempClim, dates>=firePrior & dates<=fireDate) ## need to assign/round month to last mo of season, 5 goes to 6/1/xxxx
  temp$seq<-seq(1,nrow(temp),by=1)+(-nrow(temp))
  temp$seasSeq<-paste0(temp$seas,"-",abs(temp$seq))

  # add fire info to climate DF
  temp$fireSeas<-tempFire$seas
  temp$fireSize<-tempFire$BurnBndAc
  temp$fireName<-tempFire$Incid_Name
  temp$fireYear<-tempFire$year
  temp$ecoName<-tempFire$US_L3NAME
  temp$groupVeg<-tempFire$GROUPVEG
  temp$fireSizeClass<-tempFire$fireSizeClass
  temp$fireSizeClass2<-tempFire$fireSizeClass2

  # create non fire years climatology
  seqSeas<-temp[,c("seas","seq")]
  tempClim<-merge(tempClim, temp[,c("seas","seq")], by="seas", all.x=TRUE)
  tempClim$seasSeq<-paste0(tempClim$seas,"-",abs(tempClim$seq))
  # subset out ante Fire years
  tempClim<-subset(tempClim, dates<firePrior | dates>fireDate)
  tempClim<-tempClim[order(tempClim$dates),]

  # create random distribution
  seas0<-which(tempClim$seq==0)
  #idx<-sample(seq(1:length(seas0)),1)

  cSPI<-list()
  cSPEI<-list()
  cTmean<-list()
  cVPD<-list()
  nlst<-1
  for(l in 1:length(seas0)){

    idx1<-seas0[l]
    idx2<-seas0[l]-20
    if(idx2>0){
      tempClimSub<-tempClim[c(idx1:idx2),]
      tempClimSub<-tempClimSub[order(tempClimSub$dates),]

      #3 get sequence index
      seqIdx<-c()
      k=1
      seq8<-seq(-8,0,1)
      for(j in 1:nrow(tempClimSub)){
        if(k<=length(seq8)){
          if(seq8[k]==tempClimSub$seq[j]){
            seqIdx[k]<-j
            k=k+1
          }
        }
      }
      ##
      tempClimSub<-tempClimSub[seqIdx,]
      if(nrow(tempClimSub)==length(seq8)){
        cSPI[[nlst]]<-tempClimSub$spi3
        cSPEI[[nlst]]<-tempClimSub$spei3
        cVPD[[nlst]]<-tempClimSub$anomVPDmax
        cTmean[[nlst]]<-tempClimSub$anomTmean
        nlst<-nlst+1
      }
    }
  }
  # create means/sd
  cSPI<-do.call(cbind,cSPI)
    temp$meanSPI3NF<-rowMeans(cSPI)
    temp$sdSPI3NF<-rowSd(cSPI)
  cSPEI<-do.call(cbind,cSPEI)
    temp$meanSPEI3NF<-rowMeans(cSPEI)
    temp$sdSPEI3NF<-rowSd(cSPEI)
  cVPD<-do.call(cbind,cVPD)
    temp$meanVPDmaxNF<-rowMeans(cVPD)
    temp$sdVPDmaxNF<-rowSd(cVPD)
  cTmean<-do.call(cbind,cTmean)
    temp$meanTmeanNF<-rowMeans(cTmean)
    temp$sdTmeanNF<-rowSd(cTmean)

  fireClim[[i]]<-temp
  print(i)
}

fireClim<-do.call(rbind, fireClim)

saveRDS(fireClim, file = "./data/fireClim.Rds")
#####


