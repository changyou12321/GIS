install.packages("tidyverse")
install.packages("sf")
install.packages("raster")
install.packages("dplyr")
install.packages("spData")
install.packages("tmap")
install.packages("leaflet")
install.packages("ggplot2")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("rgeos")

install.packages("RColorBrewer")
install.packages("ggspatial")
install.packages("ggthemes")


library(tidyverse)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(tmap)
library(leaflet)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(RColorBrewer)

library(tidyverse)
library(sf)
library(ggspatial)
library(ggthemes)

#load uk map from web
uk <- readRDS(gzcon(url("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_GBR_3_sf.rds")))


#remove unused column
uk <- uk[ -c(1:3,5:9, 11:16) ]
uk <- uk %>% rename(Area = NAME_3) 

#filter northern ireland
uk <- uk %>% 
  filter(NAME_1 != "Northern Ireland")

#remove name_1
uk <- uk[ -c(1) ]

#rename some area's name
uk <- uk %>% 
  mutate(Area = dplyr::recode(Area, 
                              `Aberdeen` = "Aberdeen City",
                              `Anglesey` = "Isle of Anglesey",
                              `Edinburgh` = "City of Edinburgh",
                              `Bristol` = "Bristol, City of",
                              `Dundee` = "Dundee City",
                              `Durham` = "County Durham",
                              `Glasgow` = "Glasgow City",
                              `Herefordshire` = "Herefordshire, County of",
                              `Kingston upon Hull` = "Kingston upon Hull, City of",
                              `Suffolk coastal` = "Suffolk Coastal",
                              `Saint Helens` = "St. Helens",
                              `Saint Edmundsbury` = "St Edmundsbury",
                              `Saint Albans` = "St Albans",
                              `Rhondda, Cynon, Taff` = "Rhondda Cynon Taf",
                              `Perthshire and Kinross` = "Perth and Kinross")) 

#read child obess data
data <- read.csv("child_obess.csv")

a<-merge(uk,data,by.x='Area',by.y='Area',all.x=TRUE)

a[is.na(a[,'obe']),'obe']<-0

ggplot(data = a) +
  geom_sf(aes(geometry = geometry, fill = obe,alpha=I(4/5))) +
  scale_fill_gradientn(name='Obesity',colours=brewer.pal(5,"YlGnBu"), breaks=c(-1,0.02,0.04,0.06,0.08,0.1),
                       labels=c('0-0.02','0.02-0.04','0.04-0.06','0.06-0.08','0.08-0.1','over 0.1'))+
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"),
                         style = north_arrow_nautical) +
  ggtitle(label='Child Obesity rate',subtitle = 'Miss value when value is 0')+
  theme_light()+
  theme( panel.grid=element_blank(), panel.background=element_blank(),
    axis.text=element_blank(),axis.ticks=element_blank(),
   axis.title=element_blank(),legend.position=c(0.2,0.3))


#read  data
data_adult <- read.csv("adult_16.csv")

a<-merge(a,data_adult,by.x='Area',by.y='Area',all.x=TRUE)

#male obessity number
ggplot(data = a) +
  geom_sf(aes(geometry = geometry, fill = male,alpha=I(4/5))) +
  scale_fill_gradientn(name='obesity number',colours=brewer.pal(5,"YlGnBu"), breaks=c(-1,10,20,30,40,50),
  labels=c('0-10','10 -20','20-30','30-40','40-50','over 50'))+
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"),
                         style = north_arrow_nautical) +
  ggtitle(label='Male Obesity number per 100,000 of population',subtitle = 'Miss value when value is 0')+
  theme_light()+
  theme( panel.grid=element_blank(), panel.background=element_blank(),
         axis.text=element_blank(),axis.ticks=element_blank(),
         axis.title=element_blank(),legend.position=c(0.2,0.3))


#female obessity number

ggplot(data = a) +
  geom_sf(aes(geometry = geometry, fill = female,alpha=I(4/5))) +
  scale_fill_gradientn(name='obesity number',colours=brewer.pal(6,"YlGnBu"), breaks=c(-1,10,20,30,40,50),
                       labels=c('0-10','10 -20','20-30','30-40','40-50','over 50'))+
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"),
                         style = north_arrow_nautical) +
  ggtitle(label='Female Obesity number per 100,000 of population',subtitle = 'Miss value when value is 0')+
  theme_light()+
  theme( panel.grid=element_blank(), panel.background=element_blank(),
         axis.text=element_blank(),axis.ticks=element_blank(),
         axis.title=element_blank(),legend.position=c(0.2,0.3))


#income
data_income <- read.csv("income.csv")

a<-merge(a,data_income,by.x='Area',by.y='la_name',all.x=TRUE)


ggplot(data = a) +
  geom_sf(aes(geometry = geometry, fill = total_weekly_income,alpha=I(4/5))) +
  scale_fill_gradientn(name='income',colours=brewer.pal(4,"YlGnBu"), breaks=c(0,500,700,900,1100),
  labels=c('0','500-700','700-900','900-1100','over 1100'))+
  annotation_scale(location = "bl", width_hint = 0.1) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"),
                         style = north_arrow_nautical) +
  ggtitle(label='Total Weekly income',subtitle = 'Miss value when value is 0')+
  theme_light()+
  theme( panel.grid=element_blank(), panel.background=element_blank(),
         axis.text=element_blank(),axis.ticks=element_blank(),
         axis.title=element_blank(),legend.position=c(0.2,0.3))
