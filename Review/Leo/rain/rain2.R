library(dplyr)
library(moderndive)
library(readr)
library(tidyr)
library(lubridate)
library(stringr)
library(sf)
library(tidyverse)
library(zoo)
library(scales)
library(ggplot2)
library(viridis)

RainDat <- read.csv(file="C:/Users/ecsusan/Documents/2022_DSPG_Zimbabwe/Review/Josue/data/RainDat.csv")  #Shiny app

SumRainMapDat <-function(Rmin,Rmax,yyear){
 GetSum <- data.frame(RainDat) %>% 
      filter(RainDat$year==yyear) %>% 
      group_by(Region) %>% 
      filter(newDate >= Rmin & newDate <= Rmax) %>% 
      group_by(Region)  %>% 
      dplyr::summarize(TotalRainfall = round(sum(dailyPRECIP, na.rm = TRUE),0))     
 zim_region <- st_read("C:/Users/ecsusan/Documents/2022_DSPG_Zimbabwe/Review/Josue/data/agro-ecological-regions/agro-ecological-regions.shp") #SHAPE files Shiny app
 zim_region <-dplyr::rename(zim_region, Region = nat_region)
 df_list <- list(zim_region, GetSum)
 #merge all data frames in list - note this is now a SHAPE file so can map
 map <- df_list %>% reduce(full_join, by='Region')
 ggplot(data = map) +
  geom_sf(size = 0.15, color = "black", aes(fill = TotalRainfall)) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf() +
  scale_fill_viridis(option = "viridis", direction = -1, limits = c(400, 1100), breaks=c(400,500,600,700,800,900,1000,1100)) +
  ggtitle("Total Rainfall (mm) In The Growing Season") + guides(fill=guide_legend(title="Rainfall (mm)"))
}

#DRAW TOTAL RAINFALL MAPS
#Growing Season 2010-11
yyear=2010
min <- as.Date("2010-11-20")
max <- as.Date("2011-05-31")
map201011G <- SumRainMapDat(min,max,yyear)

# #Growing Season 2016-17
yyear=2016
min <- as.Date("2016-11-20")
max <- as.Date("2017-05-31")
map201617G <- SumRainMapDat(min,max,yyear)

MeanRainMapDat <-function(Rmin,Rmax,yyear){
GetMean <- data.frame(RainDat) %>% 
  filter(RainDat$year==yyear) %>% 
  group_by(Region) %>% 
  filter(newDate >= Rmin & newDate <= Rmax) %>% 
  group_by(Region)  %>% 
  dplyr::summarize(MeanPrecipitation = round(mean(dailyPRECIP, na.rm = TRUE),0))     
zim_region <- st_read("C:/Users/ecsusan/Documents/2022_DSPG_Zimbabwe/Review/Josue/data/agro-ecological-regions/agro-ecological-regions.shp") 
zim_region <-dplyr::rename(zim_region, Region = nat_region)
df_list <- list(zim_region, GetMean)
#merge all data frames in list - note this is now a SHAPE file so can map
map <- df_list %>% reduce(full_join, by='Region')
ggplot(data = map) +
    geom_sf(size = 0.15, color = "black", aes(fill = MeanPrecipitation)) +
    xlab("Longitude") + ylab("Latitude") +
    coord_sf() +
    scale_fill_viridis(option = "viridis", direction = -1) +
    ggtitle("Total Rainfall (mm) In Month") + guides(fill=guide_legend(title="Rainfall (mm)"))
}

#December 2010
yyear=2010
min <- as.Date("2010-12-01")
max <- as.Date("2010-12-31")
map2010_12 <- MeanRainMapDat(min,max,yyear)

#January 2011
min <- as.Date("2011-01-01")
max <- as.Date("2011-01-31")
map2011_1 <- MeanRainMapDat(min,max,yyear)

#February 2011
min <- as.Date("2011-02-01")
max <- as.Date("2011-02-28")
map2011_2 <- MeanRainMapDat(min,max,yyear)

#March 2011
min <- as.Date("2011-03-01")
max <- as.Date("2011-03-31")
map2011_3 <- MeanRainMapDat(min,max,yyear)

#December 2016
yyear=2016
min <- as.Date("2016-12-01")
max <- as.Date("2016-12-31")
map2016_12 <- MeanRainMapDat(min,max,yyear) 

min <- as.Date("2017-01-01")
max <- as.Date("2017-01-31")
map2017_1 <- MeanRainMapDat(min,max,yyear)

min <- as.Date("2017-02-01")
max <- as.Date("2017-02-28")
map2017_2 <- MeanRainMapDat(min,max,yyear)

#Display maps if you want to see them
#Growing Season
map201011G
map201617G

#Monthds
map2010_12
map2011_1
map2011_2
map2011_3
map2016_12 
map2017_1
map2017_2

#(3) Histogram of average monthly rainfall from 2011 and 2017
#Data for Histograms
WideMean <- read.csv(file="C:/Users/ecsusan/Documents/2022_DSPG_Zimbabwe/Review/Josue/data/WideMean.csv")  #for Shiny app

HistRain <-function(RegionNum){
WideMean %>% # code for Shiny app
  filter(Region == RegionNum) %>% 
 ggplot(aes(fill=m, y=round(monthPRECIP,0), x=m)) + 
     geom_bar(position="dodge", stat="identity")+ 
     scale_y_continuous(limits = c(0,400)) +
     labs(color="time") +
     xlab("Region") + ylab("Precipitation (mm)") + 
     ggtitle("Average Monthly Rainfall In Region") +
     guides(fill=guide_legend(title="Month")) +
     scale_fill_viridis(discrete=TRUE, direction=-1)
}

HistRain("I")
HistRain("IIA")
HistRain("IIB")
HistRain("III")
HistRain("IV")
HistRain("V")

#Rain Spells
spell_barchart <- read.csv(file="C:/Users/ecsusan/Documents/2022_DSPG_Zimbabwe/Review/Josue/data/spell_barchart.csv")  #for Shiny app

# Grouped plot for regions in 2010-11
spell_barchart %>% # code for Shiny app
  filter(year==2010) %>%
 ggplot(aes(fill=variable, y=value, x=region)) + 
     geom_bar(position="dodge", stat="identity")+ 
     labs(color="time") +
     xlab("Regions") + ylab("Count Of Dry Spells") + 
    ggtitle("Number Of Dry Spells In The 2010-11 Growing Season") +
    guides(fill=guide_legend(title="Length Of Spell")) +
    scale_fill_viridis(discrete=TRUE, direction=-1,name = "Regions", labels = c("10 to 20 Days", ">20 Days")) 

# Grouped plot for regions in 2016-17
spell_barchart %>% # code for Shiny app
  filter(year==2016) %>%
 ggplot(aes(fill=variable, y=value, x=region)) + 
     geom_bar(position="dodge", stat="identity")+ 
     labs(color="time") +
     xlab("Regions") + ylab("Count Of Dry Spells") + 
    ggtitle("Number Of Dry Spells In The 2016-17 Growing Season") +
    guides(fill=guide_legend(title="Length Of Spell")) +
    scale_fill_viridis(discrete=TRUE, direction=-1,name = "Regions", labels = c("10 to 20 Days", ">20 Days")) 
