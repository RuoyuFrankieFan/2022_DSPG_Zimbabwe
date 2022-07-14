library("leaflet")

mypal <- colorNumeric(
  palette = "viridis",
  domain = NULL
  )

#Here is my leaflet code
#The label option creates a popup with my Region name followed by the Avg Surface Moisture value
leaflet(total1011) %>% addTiles() %>%  
  addPolygons(color = ~mypal(TotalRainfall), weight = 1, smoothFactor = 0.5, label = paste("Region -", total1011$Region,":", round(total1011$TotalRainfall, digits = 3)),
              opacity = 1.0, fillOpacity = 0.2,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = TRUE)) %>% 
  addLegend(pal = mypal,position = "bottomright",labels = c("450",'500','550','600','650','700'),values = ~(total1011$TotalRainfall),
            opacity = .6,title= paste("Total Average Rainfall (mm)"))%>%
  addPolylines(data = total1011$geometry, color = "black", opacity = 2, weight = 2)

#-----------------------------------------------------------------------------------------------------------

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
  
  
  
  
  #leaflet(total1011) %>% addTiles() %>%  
#addPolygons(color = ~mypal(TotalRainfall), weight = 1, smoothFactor = 0.5, label = paste("Region -", total1011$Region,":", round(total1011$TotalRainfall, digits = 3)),
 #           opacity = 1.0, fillOpacity = 0.2,
  #          highlightOptions = highlightOptions(color = "black", weight = 2,
   #                                             bringToFront = TRUE)) %>% 
  #addLegend (pal = mypal, position = "bottomright",values = ~(total1011$TotalRainfall),
   #          opacity = .6,title= paste("Rainfall (mm)"))%>%
  #addPolylines(data = total1011$geometry, color = "black", opacity = 2, weight = 2)
  
  
  
  

  