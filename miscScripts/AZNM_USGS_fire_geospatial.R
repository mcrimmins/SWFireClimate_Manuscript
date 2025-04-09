# look at veg types/BpS from historical fires across AZ/NM
# need RData from USGS_wildfire_perims.R (burn period research)
# MAC 09/13/23

library(raster)
library(ggplot2)
library(dplyr)

load("~/RProjects/BurnPeriodResearch/data/AZNM_USGS_perims_1878_2019.RData")

# ggplot state data
us<-getData('GADM', country='USA', level=1)
aznm<-subset(us,NAME_1 %in% c("Arizona","New Mexico"))

# read in landfire data, use Terra library
#bps<-raster("./data/landfire/LF2020_BPS_220_CONUS/LC20_BPS_220.tif")
bps<-terra::rast("./data/landfire/LF2020_BPS_220_CONUS/LC20_BPS_220.tif")
bps<- terra::project(bps, "EPSG:4326", method = "near")
terra::activeCat(bps) <- 5 # set active layer
# plot(bps)
# plot(perimsCrop, add=TRUE)
# plot(us, add=TRUE)

# crop down to AZ and NM boundaries
aznmPerims<-raster::intersect(aznm,perimsCrop)
aznmPerims$ID<-seq(1,nrow(aznmPerims@data),by=1)
aznmPerims_df<-aznmPerims@data

# plot certain years
plot(aznmPerims[aznmPerims@data$FireYear==2011,])
  plot(us, add=TRUE)

# extract data
cats<-terra::extract(bps, terra::vect(aznmPerims))
cats<-merge(cats,aznmPerims_df[,c("FireYear","ID")],by="ID")

catCounts<-cats %>% group_by(FireYear, GROUPVEG) %>%
  summarise(count=n())
catCounts$acres<-(catCounts$count*900)/4047

catCounts<-subset(catCounts, GROUPVEG %in% c("Barren-Rock/Sand/Clay",
                                             "Shrubland",
                                             "Grassland",
                                             "Conifer",
                                             "Hardwood-Conifer",
                                             "Riparian",
                                             "Open Water",
                                             "Sparse",
                                             "Hardwood"))

ggplot(catCounts, aes(FireYear,acres,fill=GROUPVEG))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c(
                             "cyan",
                             "sandybrown",
                             "goldenrod4",
                             "ivory3",
                             "goldenrod",
                             "forestgreen",
                             "olivedrab",
                             "palegreen",
                             "sienna"
                             ))+
  scale_x_continuous(breaks = seq(1900,2020,by=5))+
  ggtitle("Annual Area Burned in AZ and NM, 1895-2019 (USGS Perimeter Dataset)")
