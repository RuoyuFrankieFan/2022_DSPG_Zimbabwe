#Script for the Soil Moisture
library(dplyr)
library(ggplot2)
library(ggrepel)
library(stringr)
library(sf)
library(tidyverse)
library(viridis)
library(zoo)


#Uploading data
mydat_long <- readRDS("C:/Users/Leo Allen/Downloads/mydat_long.RDS")


#Generate categories for soil moisture
mydat_longg <- mydat_long %>% 
  mutate(
    Wet = case_when(Moisture > 25 ~ 1,
                    TRUE ~ 0),
    
    Ideal = case_when(Moisture >= 15 & Moisture <=25 ~ 1,
                      TRUE ~ 0),
    
    Dry = case_when(Moisture >10 & Moisture <15 ~ 1,
                    TRUE ~ 0),
    
    ExtremelyDry = case_when(Moisture <=10 ~ 1,
                             TRUE ~ 0),
  )

# Make sure that the data is in columns for each region. This step will group by column to get the desired wide dataset.
#Put into columns by group
mydat_long2 <- transform(mydat_longg,                                 # Create ID by group
                         ID = as.numeric(factor(region))) %>% select(-c(region)) %>% reshape(idvar = "newDate", 
                                                                                             timevar = "ID", direction = "wide")
# Now let us look at the first month of the growing season. This means we will subset our sample to the first 30 days of planting in the 2016-17 Growing Season. We set this with the min and max variables below.
# Set limits to first thirty days of the growing season: c(min, max)
min <- as.yearmon("20161119", "%Y%m")
max <- as.yearmon("20161219", "%Y%m")

# Set axis limits c(min, max) on plot
min <- as.Date("2016-11-19")
max <- as.Date("2016-12-19")


#loading the processed data to save time
df2 <- readRDS("C:/Users/Leo Allen/Downloads/soil_hist.RDS")
#mydat_long2 <- readRDS("C:/Users/Leo Allen/Downloads/soil_line.RDS")
total <- readRDS("C:/Users/Leo Allen/Downloads/soil_map.RDS")



#Making a leaflet map of the Average soil Moisture
mypal <- colorNumeric(
  palette = "viridis",
  domain = total$AvgSurfaceMoisture)

leaflet(total) %>% addTiles() %>%  
  addPolygons(color = ~mypal(AvgSurfaceMoisture), weight = 1, smoothFactor = 0.5, label = paste("Region -", total$region,":", round(total$AvgSurfaceMoisture, digits = 3)),
              opacity = 1.0, fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = TRUE)) %>% 
  addLegend(pal = mypal,position = "bottomright",values = total$AvgSurfaceMoisture,
            opacity = .6,title= paste("Average Soil Moisture (mm)"))%>%
  
  addPolylines(data = total$geometry, color = "black", opacity = 2, weight = 2,)%>% 
  setView(lat = -19.0154, lng=29.1549 , zoom =6)



#Soil Moisture Conditions In The Planting Time During The 2016-17 Growing Season - Histogram
ggplot(df2, aes(fill=time, y=value, x=region)) + 
  geom_bar(position="dodge", stat="identity")+ 
  labs(color="time") +
  xlab("Agro-ecological Region") + ylab("Number Of 3-Day periods") + 
  ggtitle("Soil Moisture Conditions In The Planting Time During The 2016-17 Growing Season") +
  guides(fill=guide_legend(title="Soil Condition")) + labs(caption = "3 Day: NASA-USDA Enhanced SMAP Global") +
  scale_fill_viridis(discrete=TRUE, direction=-1)
#3day periods within 30 days of 11/19/16 by region and Surf-soil moisture condition

#Soil Moisture: First 30 Days Of Planting Time - Line
ggplot(mydat_long2, aes(newDate, y = value, color = variable)) + 
  geom_line(aes(y = Moisture.1, col = "Region I"), size=1.25) + 
  geom_line(aes(y = Moisture.2, col = "Region IIA"), size=1.25) + 
  geom_line(aes(y = Moisture.3, col = "Region IIB"), size=1.25) + 
  geom_line(aes(y = Moisture.4, col = "Region III"), size=1.25) + 
  geom_line(aes(y = Moisture.5, col = "Region IV"), size=1.25) + 
  geom_line(aes(y = Moisture.6, col = "Region V"), size=1.25) + 
  labs(color="Agro-ecological Region") +
  xlab("Soil Moisture: First 30 Days Of Planting Time") + ylab("Surface Soil Moisture Index (mm)") + 
  ggtitle("Planting Time During The 2016-17 Growing Season") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_color_viridis(discrete = TRUE, option = "viridis") +
  scale_x_date(limits = c(min, max)) + labs(caption = "3 Day: NASA-USDA Enhanced SMAP Global")  + theme(plot.caption=element_text(hjust = 1))

#Average Soil Moisture During The First 30 days Of 2016-17 Growing Season - ggMap
ggplot(data = total) +
  geom_sf(size = 0.15, color = "black", aes(fill = AvgSurfaceMoisture)) +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf() +
  scale_fill_viridis(option = "viridis", direction = -1, limits=c(6,14), breaks=c(6,8,10,12,14), labels=c("6","8", "10", "12", "14")) +
  ggtitle("Average Soil Moisture During The First 30 days Of 2016-17 Growing Season") + labs(caption = "3 day: NASA-USDA Enhanced SMAP Global") +
  guides(fill=guide_legend(title="Average Soil Moisture (mm)"))+theme_bw()






