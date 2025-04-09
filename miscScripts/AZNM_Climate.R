# AZ-NM Climate Time series analysis for fire-climate study
# MAC 12/27/23
# code adapted from regionsMTBS_SWClimate.R

# map layers
states <- raster::getData('GADM', country='United States', level=1)
aznm<-subset(states, NAME_1=="Arizona" | NAME_1=="New Mexico")

##### extract ecoRegion climate time series -----

library(SPEI)
library(tidyverse)
library(SCI)

#load("~/RProjects/SWWildfires/data/mtbsSW_ecoreg.RData")

# combine states
ecoregSW <- raster::aggregate(aznm, 'GID_0')

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
climFiles<-as.data.frame(list.files(path = "/scratch/crimmins/PRISM/monthly/processed/SW", pattern="*.grd",full.names = TRUE, recursive = TRUE))
climFiles$fileName<-(list.files(path = "/scratch/crimmins/PRISM/monthly/processed/SW", pattern="*.grd",full.names = FALSE, recursive = TRUE))
colnames(climFiles)<-c("path","name")
climFiles<-climFiles %>%
  separate(name, sep="_", into=c("dataset", "var", "being","end"))
climFiles<-subset(climFiles, end=="2022.grd")
climFiles$path<-as.character(climFiles$path)
###
# extract time series from fire perimeter
climAnom<-list()
tictoc::tic()
ecoClim<-list()
for(j in 1:nrow(ecoregSW)){

  print(j)

  climTS<-list()
  for(i in 1:nrow(climFiles)){
    # temp raster var
    #var<-stack(climFiles$path[i])
    var<-terra::rast(climFiles$path[i])
    # extract time series
    ext<-as.data.frame(t(terra::extract(var, terra::vect(ecoregSW[j,]), ID=FALSE, raw=FALSE, fun=mean)))
    ext<-as.data.frame(ext[2:nrow(ext),])
    colnames(ext)<-climFiles$var[i]
    climTS[[i]]<-ext
    #print(climFiles$var[i])
  }
  # create fire dataframe
  climTS<-do.call(cbind, climTS)
  climTS<-cbind.data.frame(dates,climTS)
  #colnames(climTS)[1]<-c("eco1")
  # calculate SPI
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
  
  # calculate SPEI
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
             vpdmaxZ = (vpdmax-mean(vpdmax, na.rm=TRUE))/sd(vpdmax, na.rm=TRUE))
    clim3moZ<-clim3moZ[,c("dates","tmeanZ","vpdmaxZ")]
  
  
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

  ##### 12-mo Z-scores
  # 12-month moving sums/avgs
  clim12mo<-cbind.data.frame(zoo::rollapply(climTS[,c("hargreaves","prec")], FUN = sum, width = 12,
                                           fill=NA,align="right", by.column = TRUE),
                            zoo::rollapply(climTS[,c("tdmean","tmax","tmean","tmin","vpdmax")], FUN = mean, width = 12,
                                           fill=NA,align="right", by.column = TRUE))
  #colnames(clim3mo)<-paste0(colnames(clim3mo),"3mo")
  
  # calculate z-scores
  clim12moZ<-cbind.data.frame(climTS$dates,climTS$month,clim12mo)
  colnames(clim12moZ)[1:2]<-c("dates","month")
  clim12moZ<-clim12moZ %>%
    group_by(month) %>%
    mutate(tmeanZ12 = (tmean-mean(tmean, na.rm=TRUE))/sd(tmean, na.rm=TRUE),
           vpdmaxZ12 = (vpdmax-mean(vpdmax, na.rm=TRUE))/sd(vpdmax, na.rm=TRUE))
  clim12moZ<-clim12moZ[,c("dates","tmeanZ12","vpdmaxZ12")]
  #####
  
  # add spi back in
  anoms<-merge(anoms,climTS[,c("dates","spi3","spi6","spi12","spi24",
                               "spei3","spei6","spei12","spei24")], by=c("dates"))

  # add z-scores back in
  anoms<-merge(anoms, clim3moZ, by="dates")
  
  # add 12 mo z-scores back in
  anoms<-merge(anoms, clim12moZ, by="dates")
  
  
  # fixed 3-mo seasons
  anoms$seas<-cut(anoms$month,c(0,3,6,9,12))
  levels(anoms$seas) = c("JFM","AMJ","JAS","OND")
  anoms<-subset(anoms, month %in% c(3,6,9,12))

  # write to list
  ecoClim[[j]]<-anoms

  # save full anoms
  #climAnom[[j]]<-anoms

}
tictoc::toc()

save(ecoClim, file="./data/AZNM_climate_3moRoll_12moZ.RData")
#####

##### load AZ/NM climate data
load("~/RProjects/SWWildfires/data/AZNM_climate_3moRoll.RData")
anoms<-ecoClim[[1]]
anoms$year<-as.numeric(format(anoms$dates,"%Y"))

# plots
library(ggplot2)
library(cowplot)

ggplot(anoms, aes(dates,anomTmean,color=seas))+
  geom_point()+
  geom_vline(xintercept = 1984)+
  geom_hline(yintercept = 0)

ggplot(anoms, aes(dates,anomVPDmax,fill=seas))+
  geom_col()+
  geom_vline(xintercept = 1984)+
  geom_hline(yintercept = 0)+
  scale_x_date(date_breaks = "10 years",
               date_labels = "%Y", limits = c(as.Date("1895-01-01"),as.Date("2021-12-01")))+
  theme_bw()

ggplot(anoms, aes(dates,spi3,fill=seas))+
  geom_col()+
  geom_vline(xintercept = 1984)+
  geom_hline(yintercept = 0)+
  scale_x_date(date_breaks = "10 years",
               date_labels = "%Y", limits = c(as.Date("1895-01-01"),as.Date("2021-12-01")))+
  theme_bw()


ggplot(anoms, aes(year,anomVPDmax,fill=seas))+
  geom_col()+
  geom_vline(xintercept = 1984)+
  geom_hline(yintercept = 0)+
  #scale_x_date(date_breaks = "10 years",
  #             date_labels = "%Y", limits = c(as.Date("1984-01-01"),as.Date("2021-12-01")))+
  xlim(1895,2021)+
  facet_grid(seas~.)

date1<-"2010-01-01"
date2<-"2011-08-01"

p1<-ggplot(anoms, aes(dates,spi3,fill=seas))+
  geom_col()+
  geom_vline(xintercept = 1984)+
  geom_hline(yintercept = 0)+
  scale_x_date(date_breaks = "6 months",
               date_labels = "%m-%Y", limits = c(as.Date(date1),as.Date(date2)))

p2<-ggplot(anoms, aes(dates,tmeanZ,fill=seas))+
  geom_col()+
  geom_vline(xintercept = 1984)+
  geom_hline(yintercept = 0)+
  scale_x_date(date_breaks = "6 months",
               date_labels = "%m-%Y", limits = c(as.Date(date1),as.Date(date2)))

p3<-ggplot(anoms, aes(dates,vpdmaxZ,fill=seas))+
  geom_col()+
  geom_vline(xintercept = 1984)+
  geom_hline(yintercept = 0)+
  scale_x_date(date_breaks = "6 months",
               date_labels = "%m-%Y", limits = c(as.Date(date1),as.Date(date2)))

# plots are drawn with horizontal alignment
plot_grid(p1, p2, p3, labels = NA, align = "v", ncol=1)


temp<-anoms[,c("dates","seas","spi3","spei3","tmeanZ","vpdmaxZ")]
temp<-gather(temp,var,value, spi3:vpdmaxZ)

######
# seasonal climate 

date1<-"1895-01-01"
date2<-"2021-12-01"

ggplot(subset(temp, var %in% c("spi3","vpdmaxZ")), aes(dates,value,fill=var))+
  geom_bar(position = "dodge", stat="identity")+
  geom_line(aes(y=zoo::rollmean(value, 40, na.pad=TRUE, align = "right")))+
  geom_vline(xintercept = as.Date("1984-01-01"))+
  geom_hline(yintercept = 0)+
  facet_grid(seas~var)+
  scale_x_date(date_breaks = "15 years",
               date_labels = "%m-%Y", limits = c(as.Date(date1),as.Date(date2)))+
  ggtitle("AZ/NM Average Seasonal Climate")+
  theme_bw()

ggplot(subset(temp, var %in% c("spi3","vpdmaxZ")), aes(dates,value,fill=var))+
  geom_bar(position = "dodge", stat="identity")+
  geom_line(aes(x=dates, y=zoo::rollmean(value, 40, na.pad=TRUE, align = "right")),color="black")+
  geom_vline(xintercept = as.Date("1984-01-01"))+
  geom_hline(yintercept = 0)+
  facet_grid(var~.)+
  scale_x_date(date_breaks = "10 years",
               date_labels = "%m-%Y", limits = c(as.Date(date1),as.Date(date2)))+
  ggtitle("AZ/NM Average Seasonal Climate")+
  theme_bw()

ggplot(subset(temp, var %in% c("spi3","vpdmaxZ")), aes(dates,value,fill=var))+
  geom_bar(position = "dodge", stat="identity")+
  geom_line(aes(x=dates, y=zoo::rollmean(value, 40, na.pad=TRUE, align = "right"),color=var))+
  scale_color_manual(values = c("red","blue"))+
  geom_vline(xintercept = as.Date("1984-01-01"))+
  geom_hline(yintercept = 0)+
  #facet_grid(var~.)+
  scale_x_date(date_breaks = "10 years",
               date_labels = "%m-%Y", limits = c(as.Date(date1),as.Date(date2)))+
  ggtitle("AZ/NM Average Seasonal Climate")+
  theme_bw()




######


date1<-"2014-01-01"
date2<-"2015-08-01"

ggplot(temp, aes(dates,value,fill=var))+
  geom_bar(position = "dodge", stat="identity")+
  geom_vline(xintercept = 1984)+
  geom_hline(yintercept = 0)+
  scale_x_date(date_breaks = "3 months",
               date_labels = "%m-%Y", limits = c(as.Date(date1),as.Date(date2)))+
  ggtitle("AZ/NM Average Seasonal Climate")

temp<-subset(temp, var %in% c("tmeanZ","vpdmaxZ"))

date1<-"1900-01-01"
date2<-"2021-12-01"

ggplot(temp, aes(dates,value,fill=var))+
  geom_bar(position = "dodge", stat="identity")+
  geom_vline(xintercept = 1984)+
  geom_hline(yintercept = 0)+
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y", limits = c(as.Date(date1),as.Date(date2)))+
  ggtitle("AZ/NM Average Seasonal Climate")

ggplot(subset(anoms, year>=1984), aes(tmeanZ,vpdmaxZ, color=as.factor(seas)))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)



