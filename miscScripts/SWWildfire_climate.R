# extract climate/drought data for SW wildfires
# need RData from AZNM_largeFires_analysis.R
# MAC 08/29/23

library(raster)
library(dplyr)
library(tidyr)
library(SPEI)

# load wildfire data
load("~/RProjects/SWWildfires/data/AZNM_largeWildfires.RData")
# drop empty entry
fireProgList<-fireProgList[-21]

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
climAnom<-list()
fireAnom<-list()

for(j in 1:length(fireProgList)){
  
print(fireProgList[[j]][[1]]$FireName[1])
  
  climTS<-list()
  for(i in 1:nrow(climFiles)){
    # temp raster var
    var<-stack(climFiles$path[i])
    # clip, rasterize, extract vals
    clip1 <- crop(var, extent(fireProgList[[j]][[3]])) #crop to extent of polygon
    clip2 <- rasterize(fireProgList[[j]][[3]], clip1, mask=TRUE) #crops to polygon edge & converts to raster
    ext <- as.data.frame(t(getValues(clip2)))
    ext<-as.data.frame(rowMeans(ext, na.rm=T))
    colnames(ext)<-climFiles$var[i]  
    climTS[[i]]<-ext
    print(climFiles$var[i])
  }
  # create fire dataframe
  climTS<-do.call(cbind, climTS)
  climTS<-cbind.data.frame(fireProgList[[j]][[1]]$FireName[1],dates,climTS)
  colnames(climTS)[1]<-"FireName"
  # calculate SPI
  climTS$spi3<-spi(climTS$prec,3, na.rm = TRUE)$fitted
  climTS$spi6<-spi(climTS$prec,6, na.rm = TRUE)$fitted
  climTS$spi12<-spi(climTS$prec,12, na.rm = TRUE)$fitted
  # calculate SPEI
  climTS$spei3<-spei(climTS$prec-climTS$hargreaves,3, na.rm=TRUE)$fitted
  climTS$spei6<-spei(climTS$prec-climTS$hargreaves,6, na.rm=TRUE)$fitted
  climTS$spei12<-spei(climTS$prec-climTS$hargreaves,12, na.rm=TRUE)$fitted
  
  # monthly anoms
  # calculate anomalies
  anoms<-climTS[,1:11]
  meanMO<- anoms %>% group_by(month) %>%
    summarise(meanHargreaves=mean(hargreaves),
              meanPrec=mean(prec),
              meanTDmean=mean(tdmean),
              meanTmax=mean(tmax),
              meanTmean=mean(tmean),
              meanTmin=mean(tmin),
              meanVPDmax=mean(vpdmax))
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
  
  # subset to fire event
  fireMo<-as.Date(paste0(format(fireProgList[[j]][[1]]$DateAZ[1], "%Y"),"-",format(fireProgList[[j]][[1]]$DateAZ[1], "%m"),"-01"))
  firePrior<-lubridate::ymd(fireMo)-lubridate::years(3)
  temp<-subset(anoms, anoms$dates>=firePrior & anoms$dates<=fireMo)
  temp$seq<-seq(1,nrow(temp),by=1)
  fireAnom[[j]]<-temp  
  
  # save full anoms
  climAnom[[j]]<-anoms
  
}

save(climAnom, fireAnom, file="./data/AZNM_wildfire_climate.RData")

#####
# load in processed fire-climate data
load("~/RProjects/SWWildfires/data/AZNM_wildfire_climate.RData")

# combine anom list into df 
fireAnomDF<-do.call(rbind, fireAnom)

# plot some data
library(ggplot2)

ggplot(fireAnomDF, aes(seq-37,anomVPDmax,fill=FireName))+
  geom_bar(stat = "identity")+
  geom_hline(yintercept = 0)+
  #scale_fill_brewer(palette="Dark2")+
  ggtitle("Composite Antecedent Max VPD Anomaly (0-36 months prior) for top AZ/NM Fires")+
  facet_wrap(.~FireName)+
  theme_bw()
  #scale_y_continuous(breaks = seq(-3,3,0.1),limits = c(-3,3))

ggplot(fireAnomDF, aes(seq-37,anomTDmean, fill=FireName))+
  geom_bar(stat = "identity", position="dodge")+
  ggtitle("Monthly Temp Anomaly 3-years prior for top AZ/NM Fires")+
  theme_bw()
  
ggplot(fireAnomDF, aes(seq-37,anomVPDmax,fill=FireName))+
  geom_bar(stat = "identity")+
  geom_hline(yintercept = 0)+
  #scale_fill_brewer(palette="Dark2")+
  ggtitle("Antecedent monthly Precip Anom (0-36 months prior) for top AZ/NM Fires")+
  facet_wrap(.~FireName)+
  theme_bw()
  