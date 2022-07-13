library(readxl)
library(dplyr)
library(tidyverse)
library(sf)
library(ggplot2)
library(rgdal)
library(sp)
library(directlabels)
library(ggrepel)


#setwd("C:/Users/Leo Allen/Desktop/DSPG/2022_DSPG_Zimbabwe/ShinyApp")

GrSs2011 <- read_csv(paste0(getwd(),"/data/agregion indices/evi/EVI_region_GrSs2011.csv"))
GrSs2017 <- read_csv("./data/agregion indices/evi/EVI_region_GrSs2017.csv")
EVI_region_long <- read_csv("./data/agregion indices/evi/EVI_region_long.csv")

# Other Files
zim_district <- st_read("./data/shapefiles/Zim_D60.shp")  
zim_region <- st_read("./data/shapefiles/agro-ecological-regions.shp")

zim_district <- rename(zim_district, District_name = "NAME_2")
zim_region <- rename(zim_region, Region = "nat_region")


GrSs2011Line <- EVI_region_long %>% 
  filter(Month == "05"|Month == "04"|Month =="03"|Month =="02"|Month =="01"|Month =="10"|Month =="11"|Month =="12", 
         Year == 2010|Year == 2011) %>% 
  filter(!(Year == 2010 & Month == "03")) %>% 
  filter(!(Year == 2010 & Month == "04")) %>%
  filter(!(Year == 2010 & Month == "05")) %>%
  filter(!(Year == 2010 & Month == "02")) %>% 
  filter(!(Year == 2010 & Month == "01")) %>% 
  filter(!(Year == 2011 & Month == "10")) %>% 
  filter(!(Year == 2011 & Month == "11")) %>%
  filter(!(Year == 2011 & Month == "12")) %>% 
  group_by(Region, Month) %>% 
  summarise(MaxEVI = max(EVI, na.rm = TRUE)) %>% 
  mutate(GSOrder = case_when(Month =="10" ~ "1", 
                             Month =="11" ~ "2",
                             Month =="12" ~ "3",
                             Month =="01" ~ "4",
                             Month =="02" ~ "5",
                             Month =="03" ~ "6",
                             Month =="04" ~ "7",
                             Month =="05" ~ "8"))

GrSs2011Line$Month[which(GrSs2011Line$Month=="10")] <- "October"
GrSs2011Line$Month[which(GrSs2011Line$Month=="11")] <- "November"
GrSs2011Line$Month[which(GrSs2011Line$Month=="12")] <- "December"
GrSs2011Line$Month[which(GrSs2011Line$Month=="01")] <- "January"
GrSs2011Line$Month[which(GrSs2011Line$Month=="02")] <- "Febuary"
GrSs2011Line$Month[which(GrSs2011Line$Month=="03")] <- "March"
GrSs2011Line$Month[which(GrSs2011Line$Month=="04")] <- "April"
GrSs2011Line$Month[which(GrSs2011Line$Month=="05")] <- "May"

GrSs2011Line$Month <- reorder(GrSs2011Line$Month, as.numeric(GrSs2011Line$GSOrder))
GrSs2011Line$Month <- as.factor(GrSs2011Line$Month)
GrSs2011Line$Region <- as.factor(GrSs2011Line$Region)




#write.csv(GrSs2011Line, file = "eviline2011.csv")
GrSs2011Line <- read.csv("C:/Users/Leo Allen/Downloads/eviline2011.csv")

# Max EVI
GrSs2011Line %>% 
  ggplot(aes(x = Month, y = MaxEVI, group = as.factor(Region), color = as.factor(Region))) +
  geom_line()+
  theme(axis.text.x = element_text(angle = 315)) +
  scale_colour_discrete(guide = 'none') +
  scale_x_discrete(expand=c(0, 1)) +
  geom_dl(aes(label = Region), method = list(dl.combine("first.points", "last.points")), cex = 0.8) +
  scale_color_viridis_d(option = "H") +
  labs(title = "Max EVI in Zim During Growing Season 2011", color =  "Region") +
  xlab("Time(Month)") +
  ylab("Max EVI") 

#Plotly
p11 <- GrSs2011Line %>% 
  ggplot(aes(x = Month, y = MaxEVI, group = as.factor(Region), color = as.factor(Region))) +
  geom_line()+
  theme(axis.text.x = element_text(angle = 315)) +
  scale_colour_discrete(guide = 'none') +
  scale_x_discrete(expand=c(0, 1)) +
  geom_dl(aes(label = paste(GrSs2011Line$Region)), method = list(dl.combine("first.points", "last.points")), cex = 0.8) +
  scale_color_viridis_d(option = "H") +
  labs(title = "Max EVI in Zim During Growing Season 2011", color =  "Region") +
  xlab("Time(Month)") +
  ylab("Max EVI") 

ggplotly(p11)



GrSs2017Line <- read.csv("C:/Users/Leo Allen/Downloads/eviline2017.csv")


# Max EVI
GrSs2017Line %>% 
  ggplot(aes(x = Month, y = MaxEVI, group = as.factor(Region), color = as.factor(Region))) +
  geom_line()+
  theme(axis.text.x = element_text(angle = 315)) +
  scale_colour_discrete(guide = 'none') +
  scale_x_discrete(expand=c(0, 1)) +
  geom_dl(aes(label = Region), method = list(dl.combine("first.points", "last.points")), cex = 0.8) +
  scale_color_viridis_d(option = "H") +
  labs(title = "Max EVI in Zim During Growing Season 2017", color =  "Region") +
  xlab("Time(Month)") +
  ylab("Max EVI") 


p17 <- GrSs2011Line %>% 
  ggplot(aes(x = Month, y = MaxEVI, group = as.factor(Region), color = as.factor(Region))) +
  geom_line()+
  theme(axis.text.x = element_text(angle = 315)) +
  scale_colour_discrete(guide = 'none') +
  scale_x_discrete(expand=c(0, 1)) +
  geom_dl(aes(label = paste(GrSs2017Line$Region)), method = list(dl.combine("first.points", "last.points")), cex = 0.8) +
  scale_color_viridis_d(option = "H") +
  labs(title = "Max EVI in Zim During Growing Season 2011", color =  "Region") +
  xlab("Time(Month)") +
  ylab("Max EVI") 

ggplotly(p17)







































