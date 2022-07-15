library(shiny)
library(leaflet)
library(readr)
library(sf)


total1011 <- readRDS(paste0(getwd(),file = "./ShinyApp/data/agregion indices/rain/total1011.RDS"))
total1617 <- readRDS(paste0(getwd(),file = "./ShinyApp/data/agregion indices/rain/total1617.RDS"))

totalDec10 <- readRDS(paste0(getwd(),file = "./ShinyApp/data/agregion indices/rain/totalDec10.RDS"))
totalJan11 <- readRDS(paste0(getwd(),file = "./ShinyApp/data/agregion indices/rain/totalJan11.RDS"))
totalFeb11 <- readRDS(paste0(getwd(),file = "./ShinyApp/data/agregion indices/rain/totalFeb11.RDS"))

totalDec16 <- readRDS(paste0(getwd(),file = "./ShinyApp/data/agregion indices/rain/totalDec16.RDS"))
totalJan17 <- readRDS(paste0(getwd(),file = "./ShinyApp/data/agregion indices/rain/totalJan17.RDS"))
totalFeb17 <- readRDS(paste0(getwd(),file = "./ShinyApp/data/agregion indices/rain/totalFeb17.RDS"))



ui <- fluidPage(
  sidebarLayout(
    titlePanel(" Zimbabwe: Josue's Leaflet Maps"),
    navlistPanel(
      "Maps and Graphs",
      tabPanel("First",
               h3("Total Rainfall (mm) In The 2010-11 & 2016-17 Growing Season"),leafletOutput("my_leaf"),radioButtons("radio", label = h3("Radio buttons"),
                                                                                                                       choices = list("Choice 1" = 1, "Choice 2" = 2), 
                                                                                                                       selected = 1),
      ),
      tabPanel("Second",
               h3("Average Daily Rainfall (mm)", leafletOutput("my2nd_leaf"))
      ),
    
)
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
      addLegend(pal = mypal,position = "bottomleft",values = total1617$TotalRainfall, opacity = .6,
                title= paste("Total Average Rainfall")) %>%
      addLayersControl(overlayGroups = c("2010-11", "2016-17"), 
                       options = layersControlOptions(collapsed = FALSE), 
                       position = "topright")
    
    
  })
  output$my2nd_leaf <- renderLeaflet({ #add files so you can deploy- this will crash without it
    leaflet(totalFeb11) %>% addTiles() %>%
      addPolygons(color = ~mypal(totalDec10$MeanPrecipitation), weight = 1, smoothFactor = 0.5, label = paste("", totalDec10$Region, ":", round(totalDec10$MeanPrecipitation, digits = 3)),
                  opacity = 1.0, fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = TRUE), group="December 2010") %>%
      addPolygons(color = ~mypal(totalJan11$MeanPrecipitation), weight = 1, smoothFactor = 0.5, label = paste("", totalJan11$Region, ":", round(totalJan11$MeanPrecipitation, digits = 3)),
                  opacity = 1.0, fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = TRUE), group="January 2011")  %>%
      addPolygons(color = ~mypal(totalFeb11$MeanPrecipitation), weight = 1, smoothFactor = 0.5, label = paste("", totalFeb11$Region, ":", round(totalFeb11$MeanPrecipitation, digits = 3)),
                  opacity = 1.0, fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = TRUE), group="February 2011")  %>%
      addPolylines(data = total1011$geometry, color = "black", opacity = 2, weight = 2) %>% 
      addLegend(pal = mypal,position = "bottomleft",values = totalFeb11$MeanPrecipitation, opacity = .6,
                title= paste("Daily Average Rainfall")) %>% 
      addLayersControl(overlayGroups = c("December 2010", "January 2011", "Febraury 2011"), 
                       options = layersControlOptions(collapsed = FALSE), position = "topright") %>% 
      hideGroup("January 2011") %>% 
      hideGroup("Febraury 2011")
    
  })
  v <- function() {
  radioVal <- reactive({
    if(input$radio == 1){
      return(total1011$TotalRainfall)
    }
    if(input$radio == 2){
      return(total1617$TotalRainfall)
    }
      })
  }
}

shinyApp(ui, server)