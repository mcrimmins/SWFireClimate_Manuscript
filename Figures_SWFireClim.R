# SW Climate-wildfire manuscript figures
# draws from code and data in MTBS_SWClimate.R
# MAC 3/5/24
# WIN11 RStudio Version 11/12/24

##### analyze fires and mtbs fireperim climate -------
library(tidyverse)
library(ggplot2)
library(cowplot)
library(sp)

##### load data
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

# limit to 2021 and earlier fires
perimsCrop<-subset(perimsCrop, year<=2021)

# create DF
mtbsDF<-sf::st_drop_geometry(perimsCrop)
# droplevels
mtbsDF<-droplevels(mtbsDF)
#mtbsDF$month<-as.numeric(format(mtbsDF$Ig_Date,"%m"))
#mtbsDF$seas<-cut(mtbsDF$month,c(0,3,6,9,12))
#levels(mtbsDF$seas) = c("JFM","AMJ","JAS","OND")
######

##### convert acres to hectares
mtbsDF$BurnBndAc<-round(mtbsDF$BurnBndAc/2.471,0)
#####


##### load AZ/NM climate data from _process_AZNM_Climate.R
load("./data/AZNM_climate_3moRoll_12moZ.RData")
anoms<-ecoClim[[1]]
anoms$year<-as.numeric(format(anoms$dates,"%Y"))
#####

##### load processed fireClim data from MTBS_SWClimate.R
# reload fireClim data frame
fireClim <- readRDS("./data/fireClim.Rds")
#fireClim <- subset(fireClim, fireSize>=10000)
#####


###### FIGURES -----

##### FIG 1 MAP ----
# get supporting map data

ext<-raster::extent(ecoreg)*1.05
usa <- sf::st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))
world <- sf::st_as_sf(maps::map("world", fill=TRUE, plot =FALSE))
ecoregSF <- sf::st_as_sf(ecoreg)
#aznm<-subset(usa, ID %in% c("arizona","new mexico"))


##### inset map2 ----
# ggplot inset data
states <- map_data("state")
countries<-map_data("world")
countries<-subset(countries, region %in% c("Canada","Mexico"))
aznm<-subset(states, region %in% c("arizona","new mexico"))

insetmap<-ggplot() + 
  geom_polygon(data = countries, aes(x = long, y = lat, group = group), fill="grey88", color="grey", size=0.1)  +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), fill="white", color="grey", size=0.1)  +
  geom_polygon(data = aznm, aes(x = long, y = lat, group = group), fill="grey", color="black", size=0.05)  + # get the state border back on top
  coord_equal( xlim = c(-123, -69), ylim = c(24, 51))+
  theme_bw()+
  theme(panel.background = element_rect(fill = "powderblue"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        line = element_blank(),
        plot.margin=grid::unit(c(0,0,-1,-1), "mm"))
g <- ggplotGrob(insetmap)
#####


# add state labels
labs<-c("NV","CA","TX","MX")
lon<-c(-114.573669,-114.769800,-104.712067,-112.998254)
lat<-c(36.557163,34.28445,31.683050,31.579434)
stLabs<-cbind.data.frame(labs,lat,lon)
# just ecoreg map with names --- Supp material
p2<-ggplot()+
  geom_sf(data=world, aes(),color = "#2b2b2b", fill = "white", size=0.125)+
  geom_sf(data=usa, aes(),color = "#2b2b2b", fill = "white", size=0.125)+
  geom_sf(data=ecoregSF, aes(fill=US_L3NAME), color='grey75', linewidth=0.5, alpha=1)+
  scale_fill_brewer(palette="Set3", name="Ecoregion")+
  geom_sf(data=subset(perimsCrop, GROUPVEG %in% c("Conifer","Shrubland","Grassland")),
          aes(), color="black", fill=NA, linewidth=0.15)+
  coord_sf(xlim = c(ext@xmin, ext@xmax), ylim = c(ext@ymin, ext@ymax), expand = FALSE)+
  #ggtitle("AZ/NM Ecoregions")+
  xlab("")+
  ylab("")+
  theme_bw()+
  theme(legend.position = "right")

p2<-p2 + annotation_custom(grob = g, ymin = 31.15, ymax = 32.40, xmin = -114.95, xmax = -112.5)

# fire-veg map -- FIG 1
p1<-ggplot()+
  geom_sf(data=world, aes(),color = "#2b2b2b", fill = "white", size=0.125)+
  geom_sf(data=usa, aes(),color = "#2b2b2b", fill = "white", size=0.125)+
  geom_sf(data=ecoregSF, aes(), color='grey75', linewidth=0.5, alpha=1)+
  #geom_sf(data=ecoregSF, aes(color=US_L3NAME), linewidth=0.5, alpha=1)+
  geom_sf(data=subset(perimsCrop, GROUPVEG %in% c("Conifer","Shrubland","Grassland")),
          aes(fill=as.factor(GROUPVEG)), color='grey90', linewidth=0.15, alpha=1)+
  scale_fill_manual(values = c("#7570b3", "#1b9e77", "#d95f02"), name="Fire Vegetation Type")+
  #geom_sf_text(data=subset(ecoregSF,Shape_Area>1.828e+10),aes(label=US_L3NAME), size=3, check_overlap = TRUE)+
  geom_text(data = stLabs, aes(x=lon,y=lat,label=labs))+
  coord_sf(xlim = c(ext@xmin, ext@xmax), ylim = c(ext@ymin, ext@ymax), expand = FALSE)+
  #ggtitle("MTBS Fire Locations, 1984-2021")+
  xlab("")+
  ylab("")+
  theme_bw()+
  theme(legend.position = "right")
#plotly::ggplotly(p)

##### inset map2 ----
# insetmap<-ggplot()+
#   geom_sf(data=world, aes(),color = "#2b2b2b", fill = "white", size=0.125)+
#   geom_sf(data=usa, aes(),color = "#2b2b2b", fill = "white", size=0.125)+
#   geom_sf(data=aznm, aes(),color = "#2b2b2b", fill = "grey", size=0.125)+
#   coord_sf( xlim = c(-123, -69), ylim = c(24, 51))+
#   theme_bw()+
#   theme(panel.background = element_rect(fill = "powderblue"),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         line = element_blank(),
#         plot.margin=grid::unit(c(0,0,-1,-1), "mm"))
#####

pMap<-cowplot::plot_grid(p1,p2, ncol=1, align="v", labels = c('A', 'B'))
save_plot("./figs/F1_map.png", pMap, base_height = 7, base_aspect_ratio = 1.25, bg = "white")

##### Stats for TABLE 1 ----

##### fire stats by veg only ----
# veg group proportions, stats 
temp<-subset(mtbsDF, GROUPVEG %in% c("Conifer","Shrubland","Grassland"))
#temp<-subset(mtbsDF)
temp<-temp %>%
  group_by(GROUPVEG) %>%  # US_L3NAME
  summarise(nFires=n(),
            totAc=sum(BurnBndAc))%>%
  ungroup()
temp<-temp%>%
  mutate(nFires_perc=100 *(nFires/sum(nFires)),
         totAc_perc=100 *(totAc/sum(totAc)))
(round(temp$totAc_perc, 1))
######

##### fire stats by veg and ecoreg ----
# veg group proportions, stats 
temp<-subset(mtbsDF, GROUPVEG %in% c("Conifer","Shrubland","Grassland"))
temp<-temp %>%
  group_by(GROUPVEG, US_L3NAME) %>%  # US_L3NAME
  summarise(nFires=n(),
            totAc=sum(BurnBndAc))%>%
  ungroup()
temp<-temp%>%
  mutate(nFires_perc=100 *(nFires/sum(nFires)),
         totAc_perc=100 *(totAc/sum(totAc)))
temp<-temp %>%
  group_by(GROUPVEG) %>%  # US_L3NAME
  mutate(nFires_perc_veg=100 *nFires/sum(nFires),
         totAc_perc_veg=100 *totAc/sum(totAc))
# make table
totAcTable<-temp[,c("GROUPVEG","US_L3NAME","totAc")] %>% 
  pivot_wider(
    names_from = GROUPVEG,
    values_from = totAc
  )
totAcTable$totalAC<-rowSums(totAcTable[,2:4], na.rm = TRUE)
totAcTable$US_L3NAME<-as.character(totAcTable$US_L3NAME)
totAcTable[is.na(totAcTable)] <- 0

totAcTable<-rbind(totAcTable,c("TotalAC",colSums(totAcTable[,c(2:5)], na.rm = TRUE)))
totAcTable <- totAcTable %>%
  mutate(across(c(colnames(totAcTable[2:5])), as.numeric))
propAcTable<- totAcTable %>%
  mutate(across(where(is.numeric), ~ paste0("(",round((. / totAcTable$totalAC[11])*100,1)," %)") ) )
totAcTable<- totAcTable %>%
  mutate(across(where(is.numeric), ~ format(., big.mark = ",")))
combined_df <- map2_df(totAcTable[,2:5], propAcTable[,2:5], ~ paste(.x, .y, sep = " "))
totAcTable<-cbind(totAcTable$US_L3NAME, combined_df)
colnames(totAcTable)[1]<-"Ecoregion"

library(kableExtra)
kableExtra::kbl(totAcTable, align = "c") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  save_kable(file = "./figs/table_1.png")
######



##### Time series by veg type FIG 3 ----
temp<-mtbsDF %>%
  group_by(year,GROUPVEG) %>%
  summarise(nFires=n(),
            totAc=sum(BurnBndAc))
# add in perc by year
temp4<-temp %>%
  group_by(GROUPVEG) %>%  # US_L3NAME
  mutate(nFires_perc_veg=100 *nFires/sum(nFires),
         totAc_perc_veg=100 *totAc/sum(totAc))
temp4<-subset(temp4, GROUPVEG %in% c("Conifer","Grassland","Shrubland"))
# sum of top fires
temp4 %>%
  group_by(GROUPVEG) %>%
  summarize(
    top_nr = sum(tail(sort(totAc_perc_veg), 3))
  )
temp4 %>% arrange(desc(totAc)) %>% group_by(GROUPVEG) %>% slice(1:3)

# add in all group stats
temp2<- mtbsDF %>%
  group_by(year) %>%
  summarize(nFires=n(),
            totAc=sum(BurnBndAc)) %>%
  mutate( GROUPVEG = 'All' ) %>%
  ungroup() %>%
  rbind( ungroup(temp) ) %>%
  mutate( GROUPVEG = as.factor(GROUPVEG) )

temp2$ratio<-temp2$totAc/temp2$nFires
temp3<-temp2
temp2<- gather(temp2, var, value, nFires:ratio,-GROUPVEG, factor_key=TRUE)
temp5<-temp2 # save for later

# set variable names
temp2$var<-forcats::fct_recode(temp2$var,"Number of Fires"="nFires", "Total Fire Area (ha)"="totAc", "Avg Fire Size (ha)"="ratio")
# reorder factors
temp2$var<-relevel(temp2$var,"Total Fire Area (ha)")

# evaluate trends
trends1<- subset(temp2, GROUPVEG %in% c("Conifer","Grassland","Shrubland","All")) %>% group_by(GROUPVEG,var) %>%
  do(broom::tidy(lm(value ~ year, .)))
trends2<- subset(temp2, GROUPVEG %in% c("Conifer","Grassland","Shrubland","All")) %>% group_by(GROUPVEG,var) %>%
  do(broom::glance(lm(value ~ year, .)))  
trends2$sig<-ifelse(trends2$p.value<=0.05,"*",NA)

# merge in significance
temp2<-merge(temp2, trends2[,c("GROUPVEG","var","sig")], by=c("GROUPVEG","var"))

p<-ggplot(subset(temp2, GROUPVEG %in% c("Conifer","Grassland","Shrubland","All")),
          aes(year,value))+
  geom_bar(position="stack", stat="identity", color="grey28", fill="grey50")+
  #scale_fill_manual(values = c("red3","#7570b3", "#1b9e77", "#d95f02"), name="Dominant Vegetation Type")+
  geom_text(aes(x=Inf,y=Inf,hjust=1.75,vjust=1.75,label=sig), color="red", size=5)+
  #facet_wrap(.~var, ncol=1, scales="free")
  facet_grid(var~GROUPVEG, scales="free")+
  #ggtitle("Annual fire stats by vegType")+
  geom_smooth(method="lm", se=FALSE)+
  theme_bw()+
  theme(legend.position="bottom",
        axis.title.y=element_blank())
p

save_plot("./figs/F3_timeSeries.png", p, base_height = 7, base_aspect_ratio = 1.25, bg = "white")

temp2<-subset(temp3, GROUPVEG %in% c("Shrubland") & year<=2002)
mean(temp2$nFires)
mean(temp2$totAc)
mean(temp2$ratio)  
temp2<-subset(temp3, GROUPVEG %in% c("Shrubland") & year>2002)
mean(temp2$nFires)
mean(temp2$totAc)
mean(temp2$ratio)  

##### FIG 2 --- annual time series
# annual total fire ac and climate
temp<-subset(temp5, GROUPVEG=="All" & var=="totAc")
temp$logAc<-log(temp$value)
tempClim<-subset(anoms[,c("month","year","spi12","tmeanZ12","vpdmaxZ12")], month==12)
colnames(tempClim)[3:5]<-c("SPI-12","Temp-Z","VPDmax-Z")
temp<-merge(temp,tempClim, by="year")  
tempClim<-temp[,c("year","SPI-12","Temp-Z","VPDmax-Z")]
summary(tempClim)
tempClim<-tempClim %>% pivot_longer(cols=`SPI-12`:`VPDmax-Z`, names_to="var",values_to="values")  

# secondary axis with transformations
# https://stackoverflow.com/questions/78759625/ggplot-how-to-have-secondary-y-axis-independent-from-the-first
a <- (max(temp$logAc) - min(temp$logAc))/(max(tempClim$values) - min(tempClim$values))
b <- max(temp$logAc) - a * max(tempClim$values)
tempClim$values_tf <- a * tempClim$values + b
#y = (y2 - b)/a # reverse transformation

p<-ggplot()+
  geom_bar(data=temp, aes(x=year,y=logAc), stat="identity", position="identity", show.legend=FALSE, fill="grey")+
  geom_line(data=subset(tempClim,var %in% c("VPDmax-Z","SPI-12")), aes(x=year, y=values_tf, color=var), linewidth=1)+
  scale_colour_manual(values = c("#1b9e77", "#d95f02"), name="")+
  #geom_smooth(method = lm, se = FALSE)+
  #ylim(min(temp$logAc),max(temp$logAc))+
  #scale_y_continuous(sec.axis = sec_axis(~./(0.5*max(temp$logAc)), name="Z-score"))+
  scale_y_continuous(name="Area Burned log(ha)", 
                     sec.axis = sec_axis(~(. - b)/a, name="Z-score"), limits=c(8,14), oob = scales::rescale_none) +
  scale_x_continuous(breaks=seq(1984,2020, by=4))+
  geom_hline(yintercept = b, linetype = "dashed")+
  geom_smooth(data=subset(tempClim,var %in% c("VPDmax-Z","SPI-12")), aes(x=year, y=values_tf, color=var), method = "lm", se=FALSE)+
  theme_classic()+
  theme(legend.position="bottom")

save_plot("./figs/F2_annTimeSeries.png", p, base_height = 7, base_aspect_ratio = 1.75, bg = "white")

summary(tempClim)

# evaluate climate trends
trends1<- subset(tempClim,var %in% c("VPDmax-Z","SPI-12")) %>% group_by(var) %>%
  do(broom::tidy(lm(values ~ year, .)))
trends2<- subset(tempClim,var %in% c("VPDmax-Z","SPI-12")) %>% group_by(var) %>%
  do(broom::glance(lm(values ~ year, .)))  
trends2$sig<-ifelse(trends2$p.value<=0.05,"*",NA)

# evaluate log ac trends
broom::tidy(lm(logAc ~ year, data=temp))
broom::glance(lm(logAc ~ year, data=temp))

# correlations
corrMat<-cbind(subset(tempClim, var=="SPI-12")$values,subset(tempClim, var=="VPDmax-Z")$values,temp$logAc)
colnames(corrMat)<-c("SPI-12","VPDmax-Z","logAc")
PerformanceAnalytics::chart.Correlation(corrMat,
                                        histogram = TRUE, pch=19)

#####  


##### FIG 4 --- antecendant climate

##### TOP 3 YEARS -- V2 antecedent seasonal fire-climate -- multivariate boxplots -----
temp<-mtbsDF %>%
  group_by(year,GROUPVEG,seas) %>%
  summarise(nFires=n(),
            totAc=sum(BurnBndAc))
# add in perc by year
temp4<-temp %>%
  group_by(GROUPVEG) %>%  # US_L3NAME
  mutate(nFires_perc_veg=100 *nFires/sum(nFires),
         totAc_perc_veg=100 *totAc/sum(totAc))
temp4<-subset(temp4, GROUPVEG %in% c("Conifer","Grassland","Shrubland"))
# sum of top fires
temp4 %>%
  group_by(GROUPVEG,seas) %>%
  summarize(
    top_nr = sum(tail(sort(totAc_perc_veg), 3))
  )
#topYears<-temp4 %>% arrange(desc(totAc)) %>% group_by(GROUPVEG) %>% slice(1:38)
temp4<-subset(temp4, seas=="AMJ")
topYears<-temp4 %>% arrange(desc(totAc)) %>% group_by(GROUPVEG) %>% slice(1:3)
# count of AMJ years with fires by GROUPVEG
tempAllAMJ<-temp4 %>% group_by(GROUPVEG) %>% 
  summarize(nYrs=n(),
            nFires=sum(nFires),
            totAc=sum(totAc))
tempAllAMJ

# SUPP Fig 1 -- plot of year contributions ---- CLEAN UP FIGURE
topYears2<-temp4 %>% arrange(desc(totAc_perc_veg)) %>% group_by(GROUPVEG) %>% slice(1:33)
topYears2$id<-rep(c(1:33),3)
p<-ggplot(topYears2, aes(x=GROUPVEG,y=totAc_perc_veg, group=id,label=as.factor(year)))+
  geom_bar(stat = "identity",position="dodge", color="black",
           fill="#9C27B0")+
  geom_text(
    angle=90,
    position=position_dodge(width=0.9),
    hjust=-0.05
  )+
  scale_y_continuous(limits=c(0,20), breaks=seq(0,20,by=5))+
  ggtitle("Top 20 AMJ Fire Years - contribution to period of record total area burned")+
  theme_bw()

save_plot("./figs/SuppF1_topYears.png", p, base_height = 7, base_aspect_ratio = 1.75, bg = "white")
##### 

# top 3 years summary stats
topYears
tempTop3<-topYears %>% group_by(GROUPVEG) %>%
  summarise(nFires=sum(nFires),
            totAc=sum(totAc),
            nFiresPerc=sum(nFires_perc_veg),
            totAcPerc=sum(totAc_perc_veg),
            fireSize=sum(totAc)/sum(nFires)) 
tempTop3
# non-extreme n fire total
tempAllAMJ$nFires-tempTop3$nFires
tempAllAMJ$totAc-tempTop3$totAc

# non-extreme years stats
otherYears<-temp4 %>% arrange(desc(totAc)) %>% group_by(GROUPVEG) %>% slice(4:38)
otherYears<- otherYears %>% group_by(GROUPVEG) %>% 
  summarise(avgAc=round(mean(totAc, na.rm=TRUE),0),
            sdAc=round(sd(totAc, na.rm=TRUE),0),
            avgNfires=round(mean(nFires, na.rm=TRUE),0),
            sdNfires=round(sd(nFires, na.rm = TRUE),0))

# make top years table
topYrsTable<-topYears[,c("GROUPVEG","year","nFires","totAc")]
colnames(topYrsTable)<-c("Vegetation","Year","Num of Fires","Total Area Burned (ha)")
library(kableExtra)
# kableExtra::kbl(topYrsTable, align = "c") %>%
#   #kable_paper(full_width = F) %>%
#   kable_paper("striped", full_width = F) %>%
#   column_spec(1, bold = T) %>%
#   collapse_rows(columns = 1:1, valign = "top") %>%
#   kable_styling() %>%
#   save_kable(file = "table_1.png")
kableExtra::kbl(topYrsTable, align = "c") %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  save_kable(file = "./figs/table_2.html")
webshot2::webshot("./figs/table_2.html", "./figs/table_2.png", delay = 5)

# merge top years into fireClim DF
topYears$topX<-"Extreme"
topYears<-topYears[,c(1,2,8)]
temp<-merge(fireClim,topYears, by.x = c("fireYear","groupVeg"), by.y=c("year","GROUPVEG"), all.x = TRUE)
temp$topX<-ifelse(is.na(temp$topX), "Non-extreme", temp$topX)

# subset
temp<-subset(temp,
             groupVeg %in% c("Shrubland","Conifer","Grassland")
             & seq>=-4
             & fireSeas %in% c("AMJ")
             & fireSizeClass2 %in% c("G+","F"))
temp<-temp[,c("groupVeg","seq","fireSeas","fireYear","topX","spi3","spei3","vpdmaxZ","tmaxZ","tdmeanZ","tmeanZ")]
temp2<-temp
temp<- gather(temp, var, value, spi3:tmeanZ, factor_key=TRUE)

# rename SPI and VPD for facet labels
levels(temp$var)[1]<-"SPI-3"
levels(temp$var)[3]<-"VPD-max"

# Wilcoxon Sign Rank Test http://www.sthda.com/english/wiki/one-sample-wilcoxon-signed-rank-test-in-r
wTest<-temp %>% group_by(groupVeg, seq, var, topX) %>%
  summarize(wPvalue=wilcox.test(value, mu=0)$p.value,
            medVal=median(value, na.rm = TRUE),
            count=n())
wTest$sig<-ifelse(wTest$wPvalue<=0.05, "*","")


p<-ggplot(data=subset(temp,var %in% c("SPI-3","VPD-max")), #"tdmeanZ","tmaxZ"
          aes(as.factor(seq),value,fill=topX))+
  geom_boxplot(varwidth = TRUE, outlier.colour = "grey", notch=FALSE)+
  scale_fill_manual(values = c("#beaed4","#fdc086"), name="Wildfire Years")+
  #geom_violin(position = dodge)+
  #geom_boxplot(width=.1, outlier.colour=NA, position = dodge)+ 
  #stat_summary(fun.data = give.n, geom = "text")+
  geom_hline(yintercept = 0)+
  #geom_hline(yintercept=1, linetype='dotted', col = 'black')+
  #geom_hline(yintercept=-1, linetype='dotted', col = 'black')+
  facet_grid(var~groupVeg)+
  #ylim(-4,4)+
  #ggtitle(paste0("Top3 v rest antecedent climate - AMJ (with Wilcoxon Signed Rank Test)"))+
  scale_x_discrete(labels=c("-4" = "AMJ-1", "-3" = "JAS-1","-2" = "OND-1",
                            "-1" = "JFM-0","0" = "AMJ-0"))+
  scale_y_continuous(breaks=c(-3,-2,-1,0,1,2,3))+
  ylab("Z-score")+
  xlab("Season")+
  theme_bw()+
  theme(legend.position="bottom")+
  geom_text(data=subset(wTest,var %in% c("SPI-3","VPD-max")), #"tdmeanZ","tmaxZ"
            aes(as.factor(seq),3.5,label=sig), position = position_dodge(width = 0.6), size=10)

save_plot("./figs/F4_boxPlots.png", p, base_height = 7, base_aspect_ratio = 1.75, bg = "white")

tempX<-subset(temp,var %in% c("SPI-3","VPD-max"))
tempX<-tempX %>% group_by(groupVeg,seq,fireSeas,topX,var) %>%
  summarize(median=median(value, na.rm = TRUE))

#### write out data files for Zenodo repository

