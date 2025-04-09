# SW Wildfire analysis - using FOD data
# MAC 11/20/23
# adapted from burn period research fodAnalysis.R
# meta data for FOD https://www.fs.usda.gov/rds/archive/products/RDS-2013-0009.6/_metadata_RDS-2013-0009.6.html

library(dplyr)
library(tidyr)
library(SPEI)
library(tidyverse)
library(ggplot2)

# map layers
states <- raster::getData('GADM', country='United States', level=1)
#  az<-subset(states, NAME_1=="Arizona")
#  nm<-subset(states, NAME_1=="New Mexico")
aznm<-subset(states, NAME_1=="Arizona" | NAME_1=="New Mexico")
#aznm<-subset(states, NAME_1=="Arizona" | NAME_1=="New Mexico"| NAME_1=="California" | NAME_1=="Nevada" | NAME_1=="Utah" | NAME_1=="Colorado"| NAME_1=="Texas")
#  us <- getData('GADM', country='United States', level=0)
#  mx <- getData('GADM', country='Mexico', level=0)
#cn <- getData('GADM', country='Canada', level=0)
ecoreg<-rgdal::readOGR(dsn="~/RProjects/SOMs/monsoonPrecip/shapes", layer="us_eco_l3")
  ecoreg <- sp::spTransform(ecoreg,raster::crs(states))
# intersect polys
  ecoreg<-raster::intersect(ecoreg,aznm)
  
# load cropped FOD 
load("~/RProjects/BurnPeriodResearch/data/swFOD.RData")

# add in ecoreg to points
# https://gis.stackexchange.com/questions/270537/joining-attributes-by-location-for-more-than-one-located-feature-using-r
ecoreg <- sp::spTransform(ecoreg,raster::crs(fc))
# us sf to join
fc <- sf::st_join(sf::st_as_sf(fc), sf::st_as_sf(ecoreg))
# drop some columns
fc <- fc[,c(1:37,39,43,45)]
# convert to simple dataframe 
fc<-sf::as_Spatial(fc)
  fc<-fc@data

# subset to only AZ and NM fires
fc<-subset(fc, STATE %in% c("AZ","NM"))

#####
# get full table stats
ggplot(fc, aes((FIRE_SIZE_CLASS)))+
  geom_histogram(stat="count")
quantile(fc$FIRE_SIZE, p=0.995)

sum(fc[fc$FIRE_SIZE>=10000,]$FIRE_SIZE)/sum(fc$FIRE_SIZE)
length(fc[fc$FIRE_SIZE>=10000,]$FIRE_SIZE)/length(fc$FIRE_SIZE)

# fire season time series
fcTS<-subset(fc) %>% group_by(FIRE_YEAR, FIRE_SIZE_CLASS) %>%
  summarize(count=n(),
            totalAc=sum(FIRE_SIZE))

ggplot(fcTS, aes(FIRE_YEAR,totalAc,fill=FIRE_SIZE_CLASS))+
  geom_bar(position="stack", stat="identity")+
  ggtitle("Total Area Burned/Year by SIZE CLASS (AZ-NM), 1992-2020)")

ggplot(fcTS, aes(FIRE_YEAR,count,fill=FIRE_SIZE_CLASS))+
  geom_bar(position="stack", stat="identity")+
  ggtitle("Total # Fires/Year by SIZE CLASS (AZ-NM), 1992-2020)")

#####


# subset to larger fire size
fc<-subset(fc, FIRE_SIZE>=1000)

# change var types
i <- sapply(fc, is.factor)
  fc[i] <- lapply(fc[i], as.character)
fc$DISCOVERY_DATE<-as.Date(fc$DISCOVERY_DATE,format="%m/%d/%Y")
fc$CONT_DATE<-as.Date(fc$CONT_DATE,format="%m/%d/%Y")


##### add in veg cover info ####
# read in landfire data, use Terra library
#bps<-raster("./data/landfire/LF2020_BPS_220_CONUS/LC20_BPS_220.tif")
bps<-terra::rast("./data/landfire/LF2020_BPS_220_CONUS/LC20_BPS_220.tif")
bps<- terra::project(bps, "EPSG:4326", method = "near")

# extract BPS name and groupveg
terra::activeCat(bps) <- 4 # set active layer
  bpsName<-terra::extract(bps, fc[,c('LONGITUDE','LATITUDE')], ID=FALSE)
terra::activeCat(bps) <- 5 # set active layer
  groupVeg<-terra::extract(bps, fc[,c('LONGITUDE','LATITUDE')], ID=FALSE)

# extract landcover and bind to data frame
fc<-cbind.data.frame(fc,
                    as.character(bpsName$BPS_NAME),
                    as.character(groupVeg$GROUPVEG), stringsAsFactors=FALSE)
colnames(fc)[(ncol(fc)-1):ncol(fc)]<-c("bpsName","groupVeg")
#####


#####
# load PRISM data, adapted from ./WinterSummerPrecip/extractClusterTS.R
# dates 1895-2022 PRISM data
dates=seq(as.Date("1895-01-01"), as.Date("2022-12-31"), by="month")
dates<-as.data.frame(dates)
dates$month<-as.numeric(format(dates$dates, "%m"))
dates$year<-as.numeric(format(dates$dates, "%Y"))
length(unique(dates$year))

# update scratch dir with PRISM data 1895-2022 monthly precip/mean temp
# use ~/RProjects/PRISMDownload/monthyDownloadPRISM.R
# process to subset using ~/RProjects/WinterSummerPrecip/processPRISM.R
climFiles<-as.data.frame(list.files(path = "/scratch/crimmins/PRISM/monthly/processed/SW", pattern="*.grd",full.names = TRUE, recursive = TRUE))
climFiles$fileName<-(list.files(path = "/scratch/crimmins/PRISM/monthly/processed/SW", pattern="*.grd",full.names = FALSE, recursive = TRUE))
colnames(climFiles)<-c("path","name")
climFiles<-climFiles %>% 
  separate(name, sep="_", into=c("dataset", "var", "being","end"))
climFiles<-subset(climFiles, end=="2022.grd")
climFiles$path<-as.character(climFiles$path)
#####

# extract time series from fire perimeter
#climAnom<-list()
tictoc::tic()
fireAnom<-list()
for(j in 1:nrow(fc)){
  
  print(j)
  
  climTS<-list()
  for(i in 1:nrow(climFiles)){
    # temp raster var
    #var<-stack(climFiles$path[i])
    var<-terra::rast(climFiles$path[i])
    # extract time series
    ext<-as.data.frame(t(terra::extract(var, fc[j,c('LONGITUDE','LATITUDE')], ID=FALSE, raw=FALSE)))
    ext<-as.data.frame(ext[2:nrow(ext),])
    colnames(ext)<-climFiles$var[i]  
    climTS[[i]]<-ext
    #print(climFiles$var[i])
  }
  # create fire dataframe
  climTS<-do.call(cbind, climTS)
  climTS<-cbind.data.frame(fc$FIRE_NAME[j],fc$STATE[j],fc$NWCG_CAUSE_CLASSIFICATION[j],
                           fc$NWCG_GENERAL_CAUSE[j],fc$DISCOVERY_DATE[j],fc$FIRE_SIZE[j],
                           fc$groupVeg[j],fc$bpsName[j],fc$US_L3NAME[j],fc$NA_L2NAME[j],fc$NA_L1NAME[j],
                           dates,climTS)
  colnames(climTS)[1:11]<-c("FireName","State","CauseClass","Cause","discDate","fireSize","groupVeg","bpsName","eco3","eco2","eco1")
  # calculate SPI
  climTS$spi3<-spi(climTS$prec,3, na.rm = TRUE)$fitted
  climTS$spi6<-spi(climTS$prec,6, na.rm = TRUE)$fitted
  climTS$spi12<-spi(climTS$prec,12, na.rm = TRUE)$fitted
  # calculate SPEI
  climTS$spei3<-spei(climTS$prec-climTS$hargreaves,3, na.rm=TRUE)$fitted
  climTS$spei6<-spei(climTS$prec-climTS$hargreaves,6, na.rm=TRUE)$fitted
  climTS$spei12<-spei(climTS$prec-climTS$hargreaves,12, na.rm=TRUE)$fitted
  
  # 3-month moving sums/avgs
  clim3mo<-cbind.data.frame(zoo::rollapply(climTS[,c("hargreaves","prec")], FUN = sum, width = 3,
                          fill=NA,align="right", by.column = TRUE),
                         zoo::rollapply(climTS[,c("tdmean","tmax","tmean","tmin","vpdmax")], FUN = mean, width = 3,
                                        fill=NA,align="right", by.column = TRUE))
  #colnames(clim3mo)<-paste0(colnames(clim3mo),"3mo")
    
  # monthly anoms
  # calculate anomalies
  # anoms<-climTS[,6:14]
  # meanMO<- anoms %>% group_by(month) %>%
  #   summarise(meanHargreaves=mean(hargreaves),
  #             meanPrec=mean(prec),
  #             meanTDmean=mean(tdmean),
  #             meanTmax=mean(tmax),
  #             meanTmean=mean(tmean),
  #             meanTmin=mean(tmin),
  #             meanVPDmax=mean(vpdmax))
  # anoms<-merge(climTS,meanMO, by=c("month"))
  # # calculate anoms
  # anoms$anomHarg<-anoms$hargreaves-anoms$meanHargreaves
  # anoms$anomPrec<-anoms$prec-anoms$meanPrec
  # anoms$anomTDmean<-anoms$tdmean-anoms$meanTDmean
  # anoms$anomTmax<-anoms$tmax-anoms$meanTmax
  # anoms$anomTmean<-anoms$tmean-anoms$meanTmean
  # anoms$anomTmin<-anoms$tmin-anoms$meanTmin
  # anoms$anomVPDmax<-anoms$vpdmax-anoms$meanVPDmax
  # anoms <- anoms[order(anoms$dates),]

  # 3-month anoms, swap in rolling avg/sum
  anoms<-cbind.data.frame(climTS$dates,climTS$month,clim3mo)
    colnames(anoms)[1:2]<-c("dates","month")  
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
  anoms<-merge(climTS,meanMO, by=c("month"))
  # calculate anoms
  anoms$anomHarg<-anoms$hargreaves-anoms$meanHargreaves
  anoms$anomPrec<-anoms$prec-anoms$meanPrec
  anoms$anomTDmean<-anoms$tdmean-anoms$meanTDmean
  anoms$anomTmax<-anoms$tmax-anoms$meanTmax
  anoms$anomTmean<-anoms$tmean-anoms$meanTmean
  anoms$anomTmin<-anoms$tmin-anoms$meanTmin
  anoms$anomVPDmax<-anoms$vpdmax-anoms$meanVPDmax
  anoms <- anoms[order(anoms$dates),]

  # fixed 3-mo seasons
  anoms$seas<-cut(anoms$month,c(0,3,6,9,12))
  levels(anoms$seas) = c("JFM","AMJ","JAS","OND")
  anoms<-subset(anoms, month %in% c(3,6,9,12))
  
  # fire events
  # assign fire month to fixed season
  fireMo<-as.numeric(format(fc$DISCOVERY_DATE[j], "%m"))
  fireMo<-ifelse(fireMo==12 & fireMo>=10, 12,
         ifelse(fireMo>=7 & fireMo<=9, 9,
          ifelse(fireMo>=4 && fireMo<=6, 6,3)))
  fireDate<-as.Date(paste0(format(fc$DISCOVERY_DATE[j], "%Y"),"-",fireMo,"-01"))
  # subset to fire event
  firePrior<-lubridate::ymd(fireDate)-lubridate::years(2)
  temp<-subset(anoms, anoms$dates>=firePrior & anoms$dates<=fireDate) ## need to assign/round month to last mo of season, 5 goes to 6/1/xxxx
  temp$seq<-seq(1,nrow(temp),by=1)+(-nrow(temp))
  temp$seasSeq<-paste0(temp$seas,"-",abs(temp$seq))
  # assign fire season
  temp$fireSeas<-ifelse(fireMo==12, "OND",
                 ifelse(fireMo==9, "JAS",
                        ifelse(fireMo==6, "AMJ","JFM")))
  
  # write to list
  fireAnom[[j]]<-temp  
  
  # save full anoms
  #climAnom[[j]]<-anoms
  
}
tictoc::toc()

save(fireAnom, file="./data/AZNM_wildfire_climate_FOD_gt1000ac_3moRoll.RData")

#####
# load in processed fire-climate data
load("./data/AZNM_wildfire_climate_FOD_gt1000ac_3moRoll.RData")

# combine anom list into df 
fireAnomDF<-do.call(rbind, fireAnom)

# add in seasons and thin out to four seasons
#fireAnomDF$seas<-cut(fireAnomDF$month,c(0,3,6,9,12))
#levels(fireAnomDF$seas) = c("JFM","AMJ","JAS","OND")

# # subset to seasons 
# fireAnomDF<-subset(fireAnomDF, month %in% c(3,6,9,12) | seq==0)
# # create seas-yr label 
# fireAnomDF$seasYr <-ifelse(fireAnomDF$seq>(-12), paste0(fireAnomDF$seas,"-0"),
#                   ifelse(fireAnomDF$seq<(-24), paste0(fireAnomDF$seas,"-2"),
#                   paste0(fireAnomDF$seas,"-1")))

# make some plots
library(ggplot2)

# subset to different size thresholds
fireAnomDF<-subset(fireAnomDF, fireSize>=10000)

# add in fire Year
fireAnomDF$fireYear<-as.numeric(format(fireAnomDF$discDate, "%Y"))

# fire stats by subgroup
table(fireAnomDF[which(fireAnomDF$seq==0),]$groupVeg)
table(fireAnomDF[which(fireAnomDF$seq==0),]$State)

# subset on groupVeg
fireAnomDF<-subset(fireAnomDF, groupVeg %in% c("Shrubland",
                                               "Conifer",
                                               "Riparian",
                                               "Grassland"))

# fire season time series
fireTS<-subset(fireAnomDF,seq==0) %>% group_by(fireSeas,year, groupVeg) %>%
                        summarize(count=n(),
                                  totalAc=sum(fireSize))

ggplot(fireTS, aes(year,totalAc,fill=groupVeg))+
  geom_bar(position="stack", stat="identity")+
  ggtitle("Total Area Burned/Year by Veg Type (AZ-NM, 1992-2020)")

ggplot(fireTS, aes(year,count,fill=groupVeg))+
  geom_bar(position="stack", stat="identity")+
  ggtitle("Total Number >10Kac Fires/Year by Veg Type (AZ-NM, 1992-2020)")

ggplot(subset(fireAnomDF, seq==0), aes(groupVeg,spi3))+
  geom_boxplot(varwidth = TRUE)+
  geom_hline(yintercept = 0)+
  ggtitle("3-mo SPI for AZ/NM >10K ac fires, 1992-2020")

ggplot(subset(fireAnomDF, seq==0), aes(spi12,log(fireSize), color=groupVeg))+
  geom_point()+
  facet_wrap(.~fireSeas)+
  geom_vline(xintercept = 0)+
  ggtitle("Fire Event 12-mo SPI by Fire Event Season")
#plotly::ggplotly(p)


ggplot(subset(fireAnomDF), aes(seq,spi3, group=seq))+
  geom_boxplot(varwidth = TRUE)+
  geom_hline(yintercept = 0)+
  facet_grid(fireSeas~groupVeg)+
  ggtitle("Antecedent Seasonal 3-mo SPI by Veg Type~Fire Season")

# composite line plot
ggplot(subset(fireAnomDF), aes(seq,spi3, color=as.factor(fireYear), group=FireName))+
#ggplot(subset(fireAnomDF), aes(seq,spi3,color=as.factor(fireYear), group=as.factor(fireYear)))+
  #ggpointdensity::stat_pointdensity(geom = "line", size = 0.05)+
  geom_line(size=0.5, alpha=0.5)+
  #geom_line(size=1)+
  geom_hline(yintercept = 0)+
  #scale_color_gradientn(colors = c("blue", "yellow", "red"))+
  facet_grid(fireSeas~groupVeg)+
  ggtitle("Antecedent Seasonal 3-mo SPI by Veg Type~Fire Season")
  
ggplot(subset(fireAnomDF,seq==0), aes(as.factor(groupVeg), spi3))+
  geom_boxplot()+
  geom_hline(yintercept = 0)+
  facet_wrap(.~fireSeas)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("3-mo SPI by Veg Type~Fire Season")

# counts of events for contingencies
groupCts<-subset(fireAnomDF, seq==0) %>%
  group_by(fireSeas,groupVeg) %>%
  summarise(count=n())
                                  

# antecedent with t-test against zero
library(tidyverse)
# Test whether each group differs from 0
t_tests = fireAnomDF %>%
  group_by(seq,fireSeas,groupVeg) %>%
  summarise(P = t.test(spi3, mu = 0)$p.value,
            Sig = ifelse(P < 0.05, "*", ""),
            MaxWidth = max(spi3))

ggplot(subset(fireAnomDF), aes(seq,spi3, group=seq))+
  geom_boxplot(varwidth = TRUE, outlier.shape = NA)+
  geom_hline(yintercept = 0)+
  facet_grid(fireSeas~groupVeg)+
  ggtitle("Antecedent Seasonal 3-mo SPI by Veg Type~Fire Season")+
  geom_text(aes(label = Sig, y = MaxWidth + 0.75), size = 6, color="red",
          data = t_tests)

# Test whether each group differs from 0
t_tests = fireAnomDF %>%
  group_by(seq,fireSeas,groupVeg) %>%
  summarise(P = t.test(anomTmean, mu = 0)$p.value,
            Sig = ifelse(P < 0.05, "*", ""),
            MaxWidth = max(anomTmean))

ggplot(subset(fireAnomDF), aes(seq,anomTmean, group=seq))+
  geom_boxplot(varwidth = TRUE, outlier.shape = NA)+
  geom_hline(yintercept = 0)+
  facet_grid(fireSeas~groupVeg)+
  ggtitle("Antecedent Seasonal 3-mo Tmean-anom by Veg Type~Fire Season")+
  geom_text(aes(label = Sig, y = MaxWidth + 0.75), size = 6, color="red",
            data = t_tests)

# plot seasonal time series of climate

#### TO DO
# bring in fire weather with events, look at how events end...monsoon?



