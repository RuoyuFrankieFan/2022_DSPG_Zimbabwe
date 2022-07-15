library("leaflet")

mypal <- colorNumeric(
  palette = "viridis",
  domain = total1011$TotalRainfall,
  reverse = TRUE
  )

#Here is my leaflet code
#The label option creates a popup with my Region name followed by the Avg Surface Moisture value
leaflet(total1011) %>% addTiles() %>%  
  addPolygons(color = ~mypal(TotalRainfall), weight = 1, smoothFactor = 0.5, label = paste("Region -", total1011$Region,":", round(total1011$TotalRainfall, digits = 3)),
              opacity = 1.0, fillOpacity = 0.2,
              highlightOptions = highlightOptions(color = "black", weight = 2,
                                                  bringToFront = TRUE)) %>% 
  addLegend(pal = mypal,position = "bottomright",values = ~total1011$TotalRainfall, labels = c("700","600","500"),
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
  
  
#----------------------------------------------------------------------------------------------------------

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
  
  addLegend(pal = mypal,position = "bottomleft",values = totalDec10$MeanPrecipitation, opacity = .6,
            title= paste("Daily Average Rainfall"), group = "December 2010") %>%
  addLegend(pal = mypal,position = "bottomleft",values = totalJan11$MeanPrecipitation, opacity = .6,
            title= paste("Daily Average Rainfall"), group = "January 2011") %>%
  addLegend(pal = mypal,position = "bottomleft",values = totalFeb11$MeanPrecipitation, opacity = .6,
            title= paste("Daily Average Rainfall"), group = "February 2011") %>%
  addLayersControl(overlayGroups = c("December 2010", "January 2011", "February 2011"), 
                   options = layersControlOptions(collapsed = FALSE), 
                   position = "topright")






  