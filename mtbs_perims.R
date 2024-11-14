# read and process mtbs data from https://www.mtbs.gov/direct-download
# MAC 07/16/23

library(sf)
library(raster)

sf_use_s2(FALSE)

# mtbs perims, updated dataset loaded on 11/29/23
#perims<-st_read("~/RProjects/BurnPeriodResearch/data/mtbs/us_perim/mtbs_perims_DD.shp")
perims<-st_read("./data/mtbs/mtbs_perims_DD.shp") # thru part of 2024, downloaded 11/06/24
#st_geometry_type(noaa20)
st_crs(perims)
perimsCrop<-st_intersection(perims, st_set_crs(st_as_sf(as(raster::extent(-115, -102, 31, 37.35), "SpatialPolygons")), st_crs(perims)))
#noaa20crop <- noaa20crop %>% st_drop_geometry()
perimsCrop<-as(perimsCrop, 'Spatial')

# check counts of subsets
#test<-subset(perimsCrop, Ig_Date>="2000-01-01")
#length(which(test$BurnBndAc>=5000))

# save data file
save(perimsCrop, file="./data/AZNM_mtbs_perims_1984_2024.RData")
