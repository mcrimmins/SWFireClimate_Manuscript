# SW Wildfire analysis - using FOD data AND EcoRegional Analysis, other regions
# MAC 11/20/23
# adapted from burn period research fodAnalysis.R, FOD_SWClimate.R
# meta data for FOD https://www.fs.usda.gov/rds/archive/products/RDS-2013-0009.6/_metadata_RDS-2013-0009.6.html

##### CREATE FOD dataframe with ecoregions, vegtypes -----

# # map layers
# states <- raster::getData('GADM', country='United States', level=1)
# aznm<-subset(states, NAME_1=="Arizona" | NAME_1=="New Mexico")
# ecoreg<-rgdal::readOGR(dsn="~/RProjects/SOMs/monsoonPrecip/shapes", layer="us_eco_l3")
# ecoreg <- sp::spTransform(ecoreg,raster::crs(states))
# # intersect polys
# ecoreg<-raster::intersect(ecoreg,aznm)
# 
# # load cropped FOD
# load("~/RProjects/BurnPeriodResearch/data/swFOD.RData")
#   # subset to only AZ and NM fires
#   fc<-subset(fc, STATE %in% c("AZ","NM"))
# 
# # add in ecoreg to points
# # https://gis.stackexchange.com/questions/270537/joining-attributes-by-location-for-more-than-one-located-feature-using-r
# ecoreg <- sp::spTransform(ecoreg,raster::crs(fc))
# # us sf to join
# fc <- sf::st_join(sf::st_as_sf(fc), sf::st_as_sf(ecoreg))
# # drop some columns
# fc <- fc[,c(1:37,39,43,45)]
# # convert to simple dataframe
# fc<-sf::as_Spatial(fc)
# fc<-fc@data
# 
# # change var types
# i <- sapply(fc, is.factor)
# fc[i] <- lapply(fc[i], as.character)
# fc$DISCOVERY_DATE<-as.Date(fc$DISCOVERY_DATE,format="%m/%d/%Y")
# fc$CONT_DATE<-as.Date(fc$CONT_DATE,format="%m/%d/%Y")
# 
# # read in landfire data, use Terra library
# #bps<-raster("./data/landfire/LF2020_BPS_220_CONUS/LC20_BPS_220.tif")
# bps<-terra::rast("./data/landfire/LF2020_BPS_220_CONUS/LC20_BPS_220.tif")
# bps<- terra::project(bps, "EPSG:4326", method = "near")
# 
# # extract BPS name and groupveg
# terra::activeCat(bps) <- 4 # set active layer
#   bpsName<-terra::extract(bps, fc[,c('LONGITUDE','LATITUDE')], ID=FALSE)
# terra::activeCat(bps) <- 5 # set active layer
#   groupVeg<-terra::extract(bps, fc[,c('LONGITUDE','LATITUDE')], ID=FALSE)
# 
# # # extract with buffer 1km radius
# #  https://stackoverflow.com/questions/68697489/is-there-a-way-to-extact-data-from-raster-with-buffer-using-rast-function-in-ter
# #   terra::activeCat(bps) <- 5 # set active layer
# #   r<-raster::raster(bps)
# #   sp <- sp::SpatialPoints(fc[1:1000,c('LONGITUDE','LATITUDE')])
# #   sp_buffer <-sf::st_buffer(sf::st_as_sf(sp),0.01)
# #   groupVeg<-exactextractr::exact_extract(r,sp_buffer,'mode')
# #   # need code to map to groupVeg https://landfire.gov/CSV/LF2020/LF20_BPS_220.csv
# #   # https://landfire.gov/bps.php
#   
#   
# # extract landcover and bind to data frame
# fc<-cbind.data.frame(fc,
#                        as.character(bpsName$BPS_NAME),
#                        as.character(groupVeg$GROUPVEG), stringsAsFactors=FALSE)
# colnames(fc)[(ncol(fc)-1):ncol(fc)]<-c("bpsName","groupVeg")
# 
# 
# save(fc,ecoreg, file="./data/fcSW_ecoreg.RData")
# # #######

##### extract ecoRegion climate time series -----
# 
# library(SPEI)
# library(tidyverse)
# 
# load("~/RProjects/SWWildfires/data/fcSW_ecoreg.RData")
# 
# # combine ecoregions
# ecoregSW <- raster::aggregate(ecoreg, 'US_L3NAME')
#  #sp::spplot(ecoregSW, "US_L3NAME")
# 
# ###
# load PRISM data, adapted from ./WinterSummerPrecip/extractClusterTS.R
# dates 1895-2022 PRISM data
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
# extract time series from fire perimeter
#climAnom<-list()
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
#   # calculate SPEI
#   climTS$spei3<-spei(climTS$prec-climTS$hargreaves,3, na.rm=TRUE)$fitted
#   climTS$spei6<-spei(climTS$prec-climTS$hargreaves,6, na.rm=TRUE)$fitted
#   climTS$spei12<-spei(climTS$prec-climTS$hargreaves,12, na.rm=TRUE)$fitted
#   
#   # 3-month moving sums/avgs
#   clim3mo<-cbind.data.frame(zoo::rollapply(climTS[,c("hargreaves","prec")], FUN = sum, width = 3,
#                                            fill=NA,align="right", by.column = TRUE),
#                             zoo::rollapply(climTS[,c("tdmean","tmax","tmean","tmin","vpdmax")], FUN = mean, width = 3,
#                                            fill=NA,align="right", by.column = TRUE))
#   #colnames(clim3mo)<-paste0(colnames(clim3mo),"3mo")
#   
#   # 3-month anoms, swap in rolling avg/sum
#   anoms<-cbind.data.frame(climTS$dates,climTS$month,clim3mo)
#   colnames(anoms)[1:2]<-c("dates","month")  
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
######


##### analyze fires and ecoregion climate ----
library(tidyverse)
library(ggplot2)

load("./data/fcSW_ecoreg.RData")
load("./data/AZNM_ecoregion_climate_3moRoll.RData")

# look at potential reporting bias in Size Classes
# test<-subset(fc,FIRE_SIZE_CLASS=="A")
# test<-as.data.frame(table(test$SOURCE_REPORTING_UNIT_NAME,test$FIRE_YEAR))
# ggplot(test, aes(Var2,Var1, fill=Freq))+
#   geom_tile()
# test<-spread(test,Var2,Freq)

# prepare fc df
# trim fires outside AZ/NM
fc<-subset(fc, !is.na(US_L3NAME))
# drop smallest size class fires...reporting bias from Tucson/Phoenix fire depts in 2018-present
#fc<-subset(fc, FIRE_SIZE_CLASS!="A")
fc<-subset(fc, FIRE_SIZE_CLASS %in% c("F","G"))

##### summary of fc table ----
# map of ecoreg
#ecoreg$US_L3NAME<-as.factor(as.character(ecoreg$US_L3NAME))
#sp::spplot(ecoreg, "US_L3NAME")
ecoregSF <- sf::st_as_sf(ecoreg)
ggplot()+
  geom_sf(data=ecoregSF, aes(), color='black', linewidth=0.5, alpha=1)+
  #geom_sf(data=ecoregSF, aes(color=US_L3NAME), linewidth=0.1, alpha=0.75)+
  geom_point(data=subset(fc,FIRE_SIZE_CLASS!="A"), aes(LONGITUDE,LATITUDE, color=FIRE_SIZE_CLASS), size=0.1, alpha=0.5)+
  coord_sf()+
  ggtitle("FPA-FOD Fire Locations, 1992-2020")

# create summary table
temp<-fc %>%
  group_by(FIRE_YEAR,FIRE_SIZE_CLASS) %>%
  summarise(nFires=n(),
            totAc=sum(FIRE_SIZE))
temp$ratio<-temp$totAc/temp$nFires

ggplot(temp, aes(FIRE_YEAR,ratio, fill=FIRE_SIZE_CLASS))+
  geom_bar(position="stack", stat="identity")

temp<-fc %>%
  group_by(FIRE_YEAR,groupVeg) %>%
  summarise(nFires=n(),
            totAc=sum(FIRE_SIZE))

ggplot(temp, aes(FIRE_YEAR,totAc, fill=groupVeg))+
  geom_bar(position="stack", stat="identity")


#####

#### ECOREGION - CLIMATE ANALYSIS
# combine anom list into df 
ecoClim<-do.call(rbind, ecoClim)

# seasonal fire summaries
# fixed 3-mo seasons
fc$month<-as.numeric(format(fc$DISCOVERY_DATE,"%m"))
fc$year<-as.numeric(format(fc$DISCOVERY_DATE,"%Y"))
fc$seas<-cut(fc$month,c(0,3,6,9,12))
levels(fc$seas) = c("JFM","AMJ","JAS","OND")

# create summary table
ecoregSeas<-fc %>%
  group_by(seas,year,US_L3NAME) %>%
  summarise(nFires=n(),
            totAc=sum(FIRE_SIZE))

# with groupVeg
ecoregSeasVeg<-fc %>%
  group_by(seas, year, US_L3NAME, groupVeg) %>%
  summarise(nFires=n(),
            totAc=sum(FIRE_SIZE))


# groupVeg summary
ecoregVeg<-fc %>%
  group_by(US_L3NAME, groupVeg) %>%
  summarise(nFires=n(),
            totAc=sum(FIRE_SIZE))

ggplot(ecoregVeg, aes(US_L3NAME,totAc, fill=groupVeg))+
  geom_bar(position="stack", stat="identity")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  

# join summary table with antecedent climate
# https://www.r-bloggers.com/2022/07/how-to-calculate-lag-by-group-in-r/

tempClim<-ecoClim[,c("eco1","month","year","dates","seas","spi3")]

tempClim<- tempClim %>%
            group_by(eco1) %>%
            mutate(lag_1= lag(spi3, n=1, order_by=dates),
                   lag_2= lag(spi3, n=2, order_by=dates),
                   lag_3= lag(spi3, n=3, order_by=dates),
                   lag_4= lag(spi3, n=4, order_by=dates),
                   lag_5= lag(spi3, n=5, order_by=dates),
                   lag_6= lag(spi3, n=6, order_by=dates))

#tempClim<- merge(tempClim,ecoregSeas,by.x=c("seas","year","eco1"),by.y=c("seas","year","US_L3NAME"))
tempClim<- merge(tempClim,ecoregSeasVeg,by.x=c("seas","year","eco1"),by.y=c("seas","year","US_L3NAME"))

# plots
ggplot(tempClim, aes(spi3,log(totAc), color=seas))+
  geom_point()+
  facet_wrap(.~eco1)+
  geom_vline(xintercept = 0)+
  geom_smooth(method = "lm")

ggplot(ecoregSeas, aes(year,totAc,fill=US_L3NAME))+
  geom_bar(position="dodge", stat="identity")+
  geom_smooth(method="lm")

ggplot(subset(anoms, month==9), aes(year,spi12))+
  geom_line()

# stepwise regression of lagged climate
temp<-subset(tempClim, seas=="AMJ" & eco1=="Sonoran Basin and Range"
             & groupVeg=="Shrubland")
temp<-temp[,c(6:15)]
temp<-temp[,c(-8,-9)]
# stepwise regression 
# http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/
library(MASS)
# Train the model
train.control <- caret::trainControl(method = "cv", number = 10)
step.model <- caret::train(log(totAc) ~., data = temp,
                    method = "lmStepAIC", 
                    trControl = train.control,
                    trace = FALSE
)
# Model accuracy
step.model$results
# Final model coefficients
step.model$finalModel
# Summary of the model
summary(step.model$finalModel)

m<-lm(log(totAc)~lag_1+lag_2, data=temp)
