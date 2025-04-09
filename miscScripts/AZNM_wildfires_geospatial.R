# make maps, geospatial analysis of AZ/NM large wildfires
# need RData from AZNM_largeFires_analysis.R
# MAC 09/12/23

library(raster)
library(ggplot2)
library(dplyr)

load("~/RProjects/SWWildfires/data/AZNM_largeWildfires.RData")

firePerims<-list()
for(i in 1:length(fireProgList)){
  firePerims[[i]]<-fireProgList[[i]][[3]]
}

# bind Hermit's peak perims
firePerims[[20]]<-bind(firePerims[[20]],firePerims[[21]])
firePerims<-firePerims[-21]

firePerims<-do.call(bind,firePerims)
firePerims_sp<-firePerims
  firePerims_df<-firePerims@data
  firePerims_df$id<-seq(1,nrow(firePerims_df),1)

firePerims = broom::tidy(firePerims)
firePerims$id<-as.numeric(firePerims$id)
# join dfs
firePerims = dplyr::left_join(firePerims,firePerims_df,by='id')

# ggplot state data
states <- map_data("state")

# colors for categories
# random colors -- https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
qual_col_pals = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
col_vector1 = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# or
col_vector = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
# colors sampled from full set
sampCols<-seq(length(unique(firePerims$Incid_Name)),length(col_vector),by=length(unique(firePerims$Incid_Name)))

ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill=NA, color="black", size=0.1)  +
  geom_polygon(data = firePerims, aes(x = long, y = lat, group = group, fill=as.factor(Incid_Name)), color="black", alpha=0.7)  + # get the state border back on top
  #coord_fixed(xlim=c(out$meta$ll[1]-zoomLev, out$meta$ll[1]+zoomLev), ylim=c(out$meta$ll[2]-zoomLev, out$meta$ll[2]+zoomLev), ratio = 1) +
  scale_fill_manual(values=col_vector[sampCols])+
  coord_fixed(xlim=c(-115, -102.75), ylim=c(31, 37.5), ratio = 1) +
  ggtitle("Top 20 AZ/NM Largest Wildfires")

# read in landfire data, use Terra library
#bps<-raster("./data/landfire/LF2020_BPS_220_CONUS/LC20_BPS_220.tif")
bps<-terra::rast("./data/landfire/LF2020_BPS_220_CONUS/LC20_BPS_220.tif")
bps<- terra::project(bps, "EPSG:4326", method = "near")
terra::activeCat(bps) <- 5 # set active layer
  plot(bps)
  plot(firePerims_sp, add=TRUE)

# x <- terra::rasterize(terra::vect(firePerims_sp), bps, "Incid_Name")
# ct <- terra::crosstab(c(x, bps))
# ct

# calculate percentage of area burned by BpS
cats<-terra::extract(bps, terra::vect(firePerims_sp))
cats<-merge(cats,firePerims_df[,c("Incid_Name","id")],by.x="ID",by.y="id")

catCounts<-cats %>% group_by(Incid_Name, GROUPVEG) %>%
                summarise(count=n())

catCounts <- catCounts %>%                                    # Calculate percentage by group
  group_by(Incid_Name) %>%
  mutate(perc = round((count / sum(count))*100,2)) %>% 
  as.data.frame()
# merge back in fire name
#catCounts<-merge(catCounts, firePerims_df[,c("id","Incid_Name")], by.x="ID",by.y="id")
#catCounts$BPS_NAME<-as.character(catCounts$BPS_NAME)
catCounts$GROUPVEG<-as.character(catCounts$GROUPVEG)
# create other cat for small areas
#catCounts$BPS_NAME<-ifelse(catCounts$perc<=1,"other",catCounts$BPS_NAME)

# create pie charts
sampCols<-seq(length(unique(catCounts$GROUPVEG)),length(color),by=length(unique(catCounts$GROUPVEG)))

ggplot(catCounts, aes(x="", y=perc, fill=GROUPVEG))+
  geom_bar(width = 1, stat = "identity")+
  #scale_fill_manual(values=sample(col_vector1,length(unique(catCounts$GROUPVEG))))+
  scale_fill_manual(values=c("sandybrown",
                             "forestgreen",
                             "goldenrod",
                             "sienna",
                             "olivedrab",
                             "cyan",
                             "palegreen",
                             "goldenrod4",
                             "ivory3"))+
  coord_polar("y", start=0)+
  facet_wrap(.~Incid_Name)+
  theme(legend.position="right")+
  ggtitle("Dominant Veg Types Burned in Largest AZ/NM Wildfires")



               


