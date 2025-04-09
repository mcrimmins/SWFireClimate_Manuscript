# extract GRIDMET climate/drought data for SW wildfires
# need RData from AZNM_largeFires_analysis.R
# update gridmet data using ~/FireClimate/monsoonClimo/processGridMet.R
# adapted from 
# MAC 08/29/23

library(raster)
library(dplyr)
library(tidyr)
#library(SPEI)

# load wildfire data
load("~/RProjects/SWWildfires/data/AZNM_largeWildfires.RData")
# drop empty entry
fireProgList<-fireProgList[-21]

# update scratch dir with PRISM data 1895-2022 monthly precip/mean temp
# update gridmet data using ~/FireClimate/monsoonClimo/processGridMet.R
climFiles<-as.data.frame(list.files(path = "/scratch/crimmins/gridmet/update_Aug2019/processed/SW_PSA", pattern="*.grd",full.names = TRUE, recursive = TRUE))
climFiles$fileName<-(list.files(path = "/scratch/crimmins/gridmet/update_Aug2019/processed/SW_PSA", pattern="*.grd",full.names = FALSE, recursive = TRUE))
colnames(climFiles)<-c("path","name")
climFiles<-climFiles %>% 
  separate(name, sep="_", into=c("region","PSA","dataset","timestep","var", "being","end"))
climFiles<-subset(climFiles, end=="2022.grd")
climFiles$path<-as.character(climFiles$path)
#####


#####
# dates 1979-2022 GridMet data
dates=seq(as.Date("1979-01-01"), as.Date("2022-12-31"), by="day")
dates<-as.data.frame(dates)
dates$month<-as.numeric(format(dates$dates, "%m"))
dates$year<-as.numeric(format(dates$dates, "%Y"))
dates$doy<-as.numeric(format(dates$dates,"%j"))
length(unique(dates$year))

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
  
  # monthly anoms
  # calculate anomalies
  #anoms<-climTS[,1:11]
  anoms<-climTS
  meanDOY<- anoms %>% group_by(doy) %>%
    summarise(meanBI=mean(bi),
              meanERC=mean(erc),
              meanFM100=mean(fm100),
              meanFM1000=mean(fm1000),
              meanPr=mean(pr),
              meanRmin=mean(rmin),
              meanSpH=mean(sph),
              meanTmax=mean(tmmx),
              meanVS=mean(vs))
  anoms<-merge(climTS,meanDOY, by=c("doy"))
  # calculate anoms
  anoms$anomBI<-anoms$bi-anoms$meanBI
  anoms$anomERC<-anoms$erc-anoms$meanERC
  anoms$anomFM100<-anoms$fm100-anoms$meanFM100
  anoms$anomFM1000<-anoms$fm1000-anoms$meanFM1000
  anoms$anomPr<-anoms$pr-anoms$meanPr
  anoms$anomRmin<-anoms$rmin-anoms$meanRmin
  anoms$anomSpH<-anoms$sph-anoms$meanSpH
  anoms$anomTmax<-anoms$tmmx-anoms$meanTmax
  anoms$anomVs<-anoms$vs-anoms$meanVS
  
  
  # percent rank of variables
  anoms<-anoms%>%group_by(doy)%>%mutate(percRankBI=rank(bi)/length(bi),
                                        percRankERC=rank(erc)/length(erc))
  # sort by date
  anoms <- anoms[order(anoms$dates),]
  
  # subset to fire event
  temp<-subset(anoms, anoms$dates>=fireProgList[[j]][[1]][["DateAZ"]][1]-30 & anoms$dates<=fireProgList[[j]][[1]][["DateAZ"]][1]+30)
  temp$seq<-seq(-30,nrow(temp)-31,by=1)
  fireAnom[[j]]<-temp  
  
  # save full anoms
  climAnom[[j]]<-anoms
  
}

save(climAnom, fireAnom, file="./data/AZNM_wildfire_gridmet2.RData")

#####
# load in processed fire-climate data
load("~/RProjects/SWWildfires/data/AZNM_wildfire_gridmet.RData")

# combine anom list into df 
fireAnomDF<-do.call(rbind, fireAnom)

# plot some data
library(ggplot2)

ggplot(fireAnomDF, aes(seq,anomVs,fill=FireName))+
  geom_bar(stat = "identity")+
  geom_hline(yintercept = 0)+
  #scale_fill_brewer(palette="Dark2")+
  ggtitle("Daily Wind Speed for top AZ/NM Fires")+
  #facet_wrap(.~FireName)+
  theme_bw()
  #scale_y_continuous(breaks = seq(-3,3,0.1),limits = c(-3,3))

ggplot(fireAnomDF, aes(seq-37,anomTDmean, fill=FireName))+
  geom_bar(stat = "identity", position="dodge")+
  ggtitle("Monthly Temp Anomaly 3-years prior for top AZ/NM Fires")+
  theme_bw()
  
ggplot(fireAnomDF, aes(seq,percRankBI,fill=FireName))+
  geom_bar(stat = "identity")+
  geom_hline(yintercept = 0)+
  #scale_fill_brewer(palette="Dark2")+
  ggtitle("PercRankBI for top AZ/NM Fires")+
  facet_wrap(.~FireName)+
  theme_bw()
#scale_y_continuous(breaks = seq(-3,3,0.1),limits = c(-3,3))

  