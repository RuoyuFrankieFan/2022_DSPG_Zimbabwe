#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(rsconnect)
library(sf)
library(dplyr)
library(stringr)
library(tidyverse)
library(viridis)

map1 <- read.csv("./map1.csv")
zim_region <- st_read("./agro-ecological-regions/agro-ecological-regions.shp")
zim_region <-rename(zim_region, region = nat_region)

df_list <- list(zim_region, map1)
total <- df_list %>% reduce(full_join, by='region')

ui <- fluidPage(
    titlePanel("Zimbabwe: Ari's Leaflet Maps"),
    sidebarLayout(
        sidebarPanel(p("Sidebar 1")),
        mainPanel(strong("Surface Soil moisture (mm) In The 2016-17 Growing Season"),leafletOutput("my_leaf"))
    )
)


server <- function(input, output, session){
    
    output$my_leaf <- renderLeaflet({
        mypal <- colorNumeric(
            palette = "viridis",
            domain = NULL,
            reverse = TRUE)
        
        leaflet(total) %>% addTiles() %>%
            addPolygons(color = ~mypal(total$AvgSurfaceMoisture), weight = 1, smoothFactor = 0.5, label = paste("Region ", total$region, ":", round(total$AvgSurfaceMoisture, digits = 3)),
                        opacity = 1.0, fillOpacity = 0.5,
                        highlightOptions = highlightOptions(color = "black", weight = 2,
                                                            bringToFront = TRUE), group="2016-17") %>%
            addPolylines(data = total$geometry, color = "black", opacity = 2, weight = 2) %>% 
            addLegend(pal = mypal,position = "bottomleft",values = total$AvgSurfaceMoisture, opacity = .6,
                      title= paste("Average Soil Moisture During The First 30 days Of 2016-17 Growing Season")) %>% 
            addLayersControl(baseGroups = c("2016-17"), 
                             options = layersControlOptions(collapsed = FALSE), position = "topright") 
        
        
    })
    
}

shinyApp(ui, server)
