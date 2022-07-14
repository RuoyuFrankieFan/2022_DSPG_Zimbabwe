library(shiny)
library(leaflet)
library(readr)
library(sf)

#total1011 <- readRDS(file = "./Demo/data/total1011.RDS")

total1011 <- readRDS(paste0(getwd(),file = "/data/total1011.RDS"))

total1617 <- readRDS(paste0(getwd(),file = "/data/total1617.RDS"))


#total1617 <- readRDS(file = "./Demo/data/total1617.RDS")


ui <- fluidPage(
  titlePanel("Zimbabwe: Josue's Leaflet Maps"),
  sidebarLayout(
    sidebarPanel(p("Somthing Here")),
    mainPanel(strong("Total Rainfall (mm) In The 2010-11 & 2016-17 Growing Season"),leafletOutput("my_leaf"))
)
)


server <- function(input, output, session){
  
  output$my_leaf <- renderLeaflet({
    mypal <- colorNumeric(
      palette = "viridis",
      domain = NULL,
      reverse = TRUE)
    
    leaflet(total1011) %>% addTiles() %>%
      addPolygons(color = ~mypal(total1011$TotalRainfall), weight = 1, smoothFactor = 0.5, label = paste("", total1011$Region, ":", round(total1011$TotalRainfall, digits = 3)),
                  opacity = 1.0, fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = TRUE), group="2010-11") %>%
      addPolygons(color = ~mypal(total1617$TotalRainfall), weight = 1, smoothFactor = 0.5, label = paste("", total1617$Region, ":", round(total1617$TotalRainfall, digits = 3)),
                  opacity = 1.0, fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = TRUE), group="2016-17")  %>% 
      addPolylines(data = total1011$geometry, color = "black", opacity = 2, weight = 2) %>% 
      addLegend(pal = mypal,position = "bottomleft",values = total1011$TotalRainfall, opacity = .6,
                title= paste("Total Average Rainfall")) %>% 
      addLayersControl(baseGroups = c("2010-11", "2016-17"), 
                       options = layersControlOptions(collapsed = FALSE), position = "topright") %>% 
      hideGroup("2016-17")
    
    
  })
  
}

shinyApp(ui, server)