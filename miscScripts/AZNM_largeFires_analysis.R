# AZ/NM large fires for Aridity/Wildfire project
# analyze fire progression with RAWS/burn period data
# using mtbs perims, adapted from eventAnalysis.R
# adapted from eventAnalysis_mtbsPerims.R
# MAC 08/9/23

library(raster)
library(geosphere)
library(ggplot2)


# load fire progression data from fireProgression_loop_mtbs.R
#load("./data/fireProgression_Stats_mtbs_perims_gt50K.RData")
load("~/RProjects/BurnPeriodResearch/data/fireProgression_Stats_mtbs_perims_gt10K.RData")

# scan for empty fires, thin out list
temp<-c()
names<-c()
for(i in 1:length(fireProgList)){
  temp[i]<-fireProgList[[i]][[1]]$DateAZ[1]
  names[i]<-fireProgList[[i]][[1]]$FireName[1]
}
#fireProgList<-fireProgList[which(!is.na(temp))] 

# subset to AZ/NM fires
topten<-c("WALLOW","RODEO","CAVE CREEK COMPLEX","HORSESHOE 2","BUSH","TELEGRAPH","WOODBURY",
          "BIGHORN","WILLOW","ASPEN","HERMITS PEAK","BLACK","WHITEWATER-BALDY","LAS CONCHAS",
          "SILVER","DONALDSON","DRY LAKE COMPLEX (DRY LAKES)","PASCO","MCDONALD","PONIL COMPLEX")

fireProgList<-fireProgList[which(names %in% topten)]

# save AZ/NM large fire data list
save(fireProgList, file="./data/AZNM_largeWildfires.RData")

# load burnperiod climo dataset from burnPeriodClimo.R
load("~/RProjects/BurnPeriodResearch/data/burnClimoList.RData")

# get station list from burnList
stations<-do.call(rbind, lapply(burnList,function(x) x[1,c("STA_NAME","LATITUDE","LONGITUDE")]))

# find closest RAWS to fire and attached burn period hours
# temporary RAWS station list for distances
geoStns<-stations[,c("LONGITUDE","LATITUDE")]

# loop through events and attach RAWS data
fireEventsRAWS<-list()
for(i in 1:length(fireProgList)){
  
  # find closest RAWS 
  fireCenter<-rgeos::gCentroid(fireProgList[[i]][[3]])
  fireCenter<-c(fireCenter@coords[1],fireCenter@coords[2])
  distances<-geosphere::distGeo(fireCenter, geoStns)/1000
  ranking<-rank(distances, ties.method = "first")
  
  # get data from closest RAWS dataframe
  tempDF<-burnList[[which.min(ranking)]]
  tempEvent<-fireProgList[[i]][[1]]
  tempDF<-merge(tempEvent,tempDF, by.x="DateAZ",by.y="date")
  
  if(nrow(tempDF)==0){
    tempDF[1,]<-rep(NA,ncol(tempDF))
  }else{
  }
  tempDF$RAWSdist<-distances[which.min(ranking)]  
  tempDF$cum20Bhrs<-cumsum(tempDF$bhrs20)
  # put data into list
  fireEventsRAWS[[i]]<-tempDF
}

fireEventsRAWS<-do.call(rbind, fireEventsRAWS)
fireEventsRAWS<-subset(fireEventsRAWS, !is.na(DateAZ))
# drop fires with only one observation
# filter extreme daily spread rates
fireCounts<-as.data.frame(table(fireEventsRAWS$FireName))
length(unique(fireCounts$Var1))

# fire event group stats
library(dplyr)
eventStats<- fireEventsRAWS %>% group_by(FireName) %>%
                                  summarize(date=first(DateAZ),
                                            month=first(month),
                                            year=first(year),
                                            days=n(),
                                            maxAc=max(cumSum),
                                            maxDayAc=max(acres),
                                            maxFRP=max(maxFRP),
                                            meanFRP=mean(sumFRP/countFRP),
                                            meanMaxT=round(mean(maxT),1),
                                            meanMinDP=round(mean(minDP),1),
                                            meanMinRH=round(mean(minRH),1),
                                            meanMaxWS=round(mean(maxWS),1),
                                            meanBhrs20=round(mean(bhrs20),1),
                                            meanMaxVPD=round(mean(maxVPD),1),
                                            meanBhrs20_med_anom=round(mean(bhrs20_med_anom),1),
                                            meanMaxFFWI=round(mean(maxFFWI),1),
                                            meanMaxHDW=round(mean(maxHDW),1),
                                            RAWSdist=round(first(RAWSdist),1),
                                            RAWSelev=first(elev))

eventStats$moDate<-as.Date(paste0(eventStats$year,"-",eventStats$month,"-01"))

ggplot(eventStats, aes(x=year,y=(maxAc),fill=as.factor(FireName)))+
  geom_bar(position="stack", stat="identity")+
  ggtitle("Total Acres/year - Top 10 largest AZ/NM Wildfires")

ggplot(eventStats, aes(maxAc, meanMaxVPD, color=FireName))+
  geom_point()


# map fire progression with burn hours
# combine into one raster layer and plot
for(i in 1:length(fireProgList)){
 print(fireProgList[[i]][[1]]$FireName[1])
}

i=2
fireProg<-fireProgList[[i]][[4]]
plot(fireProg)
rasterVis::levelplot(fireProg, margin=FALSE, main=fireProgList[[i]][[1]]$FireName[1], par.settings = rasterVis::PuOrTheme)

tempDF<-subset(fireEventsRAWS, FireName==fireProgList[[i]][[1]]$FireName[1])
tempDF<-tempDF[,c("doy","bhrs20")]
#tempDF<-tempDF[,c("doy","maxHDW")]
fireProg[fireProg>tempDF$doy[nrow(tempDF)]]<-NA

rc <- reclassify(fireProg, as.matrix(tempDF))
 rasterVis::levelplot(rc, margin=FALSE, main=fireProgList[[i]][[1]]$FireName[1])
 
# plot fire event info
 
 fireEvent<-subset(fireEventsRAWS, FireName=="TELEGRAPH")
 
 fireEvent<-fireEvent[c("DateAZ","maxFRP","acres","bhrs20","bhrs10","minRH",
                        "maxVPD","minDP","maxWS","maxT","maxFFWI","maxHDW",
                        "bhrs20_avg_anom")]
 fireEventLong<- tidyr::gather(fireEvent,variable,value,2:13)
 
 ggplot(fireEventLong, aes(DateAZ, value))+
   geom_line()+
   facet_wrap(~variable, scales = "free", ncol=2)+
   ggtitle("Telegraph Fire Daily Variables")
 
##### plot top 10 fires for AZ/NM ----
# RAWS or other daily fire weather anoms 
# USDM intersection
# GridMet metrics
# PRISM SPI, SPEI

 
 