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
library(tidyverse)
library(sf)
library(ggthemes)
library(RColorBrewer)
library(sjmisc)
library(shinythemes)
library(DT)
library(data.table)
library(rsconnect)
library(shinycssloaders)
library(readxl)
library(readr)
library(rgdal)
library(stringr)
library(shinyjs)
library(dplyr)
library(sf)
library(gpclib)
library(maptools)
library(shinydashboard)
library(ggpolypath)
library(ggplot2)
library(plotly)
library(ggrepel)
library(hrbrthemes)
library(rmapshaper)
library(magrittr)
library(zoo)
library(viridis)

#setwd and other references to local files must be commented out for app to run properly
#setwd("~/Ari/DSPG/zim/2022_DSPG_Zimbabwe/ShinyApp")
MapDataPre <- read.csv("data/agregion/soil/SoilMapPlotData.csv")
BarData <- read.csv("data/agregion/soil/SoilBarPlotData.csv")
LineData <- read.csv("data/agregion/soil/SoilLinePlotData.csv")
zim_region <- st_read("data/shapefiles/agro-ecological-regions.shp")
zim_region <-rename(zim_region, region = nat_region)

MapDataTwo <- list(zim_region, MapDataPre)
MapDataFin <- MapDataTwo %>% reduce(full_join, by='region')

# Set axis limits c(min, max) on plot
min <- as.yearmon("20161119", "%Y%m")
max <- as.yearmon("20161219", "%Y%m")
min <- as.Date("2016-11-19")
max <- as.Date("2016-12-19")


## CAPTIONS---------------------------------------------------------------------

slider_caption = "slidecap"
urban_rural_caption = "urbanruralcap"
level_caption = "levelcap"
c_g_caption = "c_g_cap  "



ui <- navbarPage(title = "Zimbabwe",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                 useShinyjs(),
                 
                 ## Tab Overview -----------------------------------------------------------
                 tabPanel("Overview", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   br(""),
                                   h1(strong("Ari's shiny")),
                                   # fluidRow(style = "margin: 2px;",
                                   #          img(src = "Zimbabwe_Flag.png", height="100", width="200", alt="Image", style="display: block; margin-left: auto; margin-right: auto; border: 1px solid #000000;")),
                                   h4("Data Science for the Public Good Program"),
                                   h4("Virginia Tech"),
                                   h4("Department of Agricultural and Applied Economics")
                                   
                          ),
                          
                          fluidRow(style = "margin: 6px;",
                                   column(4,
                                          h2(strong("Project Overview"), align = "center"),
                                          p("Insert overview 1"),
                                          p("Insert overview 2"),
                                          p(strong(" strong"), " inbetween strong code ", strong("strong."))
                                          
                                   ),
                                   column(4,
                                          h2(strong("Introduction to Zimbabwe"), align = "center"),
                                          p("Intro 1"), 
                                          p("Intro 2"),
                                          p("Intro 3")),
                                   
                                   column(4,
                                          h2(strong("Recent History"), align = "center"),
                                          p("History 1"))
                                   
                                   
                                   
                                   
                          ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: July 2022'))))
                 ),
                 
                 # Tab MPI Ranking-----------------
                 navbarMenu("Soil Moisture",
                            ### 91 DIstricts ranking--------
                            tabPanel("Surface Soil Moisture",
                                     
                                     tabsetPanel(
                                         tabPanel("Background",
                                                  fluidRow(style = "margin: 2px;",
                                                           align = "center",
                                                           br(""),
                                                           h1(strong("Why Surface Soil Moisture?")),
                                                           p("Appropriate Surface soil moisture levels are necessary for the success of planting and harvesting activities for most crops
                                                             with too little soil moisture during planting stifling the seed germination
                                                             and too much soil moisture preventing fieldwork or heavy machinery access to the field (Bolten, Sazib, & Mladenova, 2018). 
                                                             Because most planting activities take place during the first 30 days of the growing season,
                                                             this is the time period we have chosen to focus on for the Surface soil moisture section of our study.")
                                                           
                                                  ),
                                                  fluidRow(align = "center",
                                                           p(tags$small(em("References"))),
                                                           p(tags$small(em("Bolten, J. D., Sazib, N., & Mladenova, I. E. (2018). *Surface_Soil_Moisture_SMAP.pdf*. 
                                                   NASA Goddard Space Flight Center Retrieved from 
                                                   <https://gimms.gsfc.nasa.gov/SMOS/SMAP/SoilMoisture_Profile_SMAP.pdf>"
                                                           ))))
                                         ),
                                         tabPanel("Average Soil Moisture",
                                                  fluidRow(
                                                      box(width = 8,
                                                          withSpinner(leafletOutput("MapGraph", height = 950))
                                                      ),
                                                      box(withMathJax(),
                                                          width = 4,
                                                          title = "Description",
                                                          p("This visualization shows the average surface soil moisture (in mm) by Zimbabwe's natural regions. 
                                                            The average is taken over the first 30 days of the 2016-17 growing season, which takes place from November 19th to December 19th of 2016. 
                                                            From the visualization we can see that regions I, IIa, IIb, and III have dry surface soil moisture (10-15mm),
                                                            while regions IV and V have extremely dry surface soil moisture (>10mm). These soil moisture levels suggest that while farmers 
                                                            in all regions of Zimbabwe are likely to experience stifled germination upon planting during the 2016/2017 growing season,
                                                            farmers in regions IV and V are likely to be more impacted than their counterparts in the other regions."))
                                                      
                                                  )),
                                         
                                         tabPanel("Soil Moisture at Planting",
                                                  fluidRow(
                                                      box(width = 8,
                                                          withSpinner(plotOutput("BarGraph", height = 950))
                                                      ),
                                                      box(withMathJax(),
                                                          width = 4,
                                                          title = "Description",
                                                          p("This Grouped Bar chart shows the number of 3-day periods by region that fall within each of the four soil 
                                                          condition categories. The number of 3-day periods is taken over the first 30 days of the 2016-17 growing season,
                                                          which takes place from November 19th to December 19th of 2016. From this visualization we can see that none of the regions experienced any wet periods,
                                                          and Region V is unique in not experiencing any ideal periods. Furthermore, Regions I through III all had either four or five ideal 3-day periods,
                                                          while Region IV only had two. This aligns with the previous visualization's findings of Regions I through III 
                                                            having more soil moisture on average than regions IV and V."))
                                                      
                                                  )),
                                         
                                         tabPanel("Soil Moisture at Planting Times",
                                                  fluidRow(
                                                      box(width = 8,
                                                          withSpinner(plotOutput("LineGraph", height = 950))
                                                      ),
                                                      box(withMathJax(),
                                                          width = 4,
                                                          title = "Description",
                                                          p("This line chart shows by region the surface soil moisture in mm over the first 30 days of the 2016-17 growing season,
                                                            which takes place from November 19^th^ to December 19^th^ of 2016. From this visualization we can see that the ranking
                                                            of soil moisture levels by region remains largely consistent over the time period, the difference between the region
                                                            with the highest soil moisture and the region with the lowest roughly doubles over the first 30 days of the growing season.
                                                            In addition, while regions I -- III experience soil moisture levels above the extremely dry threshold (10mm) as early
                                                            as November 24th, regions IV and V do not reach those levels until December 9th."))
                                                      
                                                  ))
                                     )),
                            
                            
                            
                            ### DIstricts ranking--------
                            tabPanel("Subsurface Soil Moisture",
                                     #tabName = "rank_60",
                                     tabsetPanel(
                                         tabPanel("Ex.1",
                                                  fluidRow(
                                                      box(width = 8,
                                                          withSpinner(plotlyOutput("M0_ranking", height = 950))
                                                      ),
                                                      box(withMathJax(),
                                                          width = 4,
                                                          title = "Description",
                                                          p("These graphs won't work and are just for example")),
                                                      box(sliderInput("M0_k_threshold", strong("k-Threshold"), 1, 9, 3),
                                                          width = 4,
                                                          footer = slider_caption)
                                                  )),
                                         
                                         tabPanel("Ex.2",
                                                  fluidRow(
                                                      box(width = 8,
                                                          withSpinner(plotlyOutput("M1_ranking", height = 950))
                                                      ),
                                                      box(withMathJax(),
                                                          width = 4,
                                                          title = "Description",
                                                          p("These graphs won't work and are just for example")),
                                                      box(sliderInput("M1_k_threshold", strong("k-Threshold"), 1, 9, 3),
                                                          width = 4,
                                                          footer = slider_caption)
                                                  )),
                                         
                                         tabPanel("Ex.3",
                                                  fluidRow(
                                                      box(width = 8,
                                                          withSpinner(plotlyOutput("M2_ranking", height = 950))
                                                      ),
                                                      box(withMathJax(),
                                                          width = 4,
                                                          title = "Description",
                                                          p("These graphs won't work and are just for example")),
                                                      box(sliderInput("M2_k_threshold", strong("k-Threshold"), 1, 9, 3),
                                                          width = 4,
                                                          footer = slider_caption)
                                                  ))
                                     ))
                            
                            
                 ),
                 ## Tab EVI------------------------------------------------
                 tabPanel("EVI", 
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   h1(strong("Insert EVI charts"), align = "center"),
                                   br(),
                                   h4(strong("VT Data Science for the Public Good (this is for example)"), align = "center"),
                                   p("The", a(href = 'https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html', 'Data Science for the Public Good (DSPG) Young Scholars program', target = "_blank"),
                                     "is a summer immersive program offered by the", a(href = 'https://aaec.vt.edu/index.html', 'Virginia Tech Department of Agricultural and Applied Economics'), 
                                     "In its second year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges 
                                               around critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to 
                                               determine how information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program highlights, 
                                               how to apply, and our annual symposium, please visit", 
                                     a(href = 'https://aaec.vt.edu/content/aaec_vt_edu/en/academics/undergraduate/beyond-classroom/dspg.html#select=1.html', 'the official VT DSPG website.', target = "_blank")),
                                   p("", style = "padding-top:10px;")
                          ),
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   column(6, align = "center",
                                          h4(strong("(this is for example)")),
                                          p("", style = "padding-top:10px;"),
                                          img(src = "team-yang.png", style = "display: inline;  border: 0px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-sambath.jpg", style = "display: inline; border: 0px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-atticus.jpg", style = "display: inline; border: 0px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-matt.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p("", style = "padding-top:10px;"),
                                          p(a(href = 'https://www.linkedin.com/in/yang-cheng-200118191/', 'Yang Cheng', target = '_blank'), "(Virginia Tech, Agricultural and Applied Microeconomics);"),
                                          p(a(href = 'https://www.linkedin.com/in/sambath-jayapregasham-097803127/', 'Sambath Jayapregasham', target = '_blank'), "(Virginia Tech, Agricultural and Applied Microeconomics);"),
                                          p(a(href = 'https://www.linkedin.com/in/atticus-rex-717581191/', 'Atticus Rex', target = '_blank'), "(Virginia Tech, Computational Modeling and Data Analytics);"),
                                          p( a(href = 'https://www.linkedin.com/in/matthew-burkholder-297b9119a/', 'Matthew Burkholder', target = '_blank'), "(Virginia Tech, Philosophy, Politics, & Economics)."),
                                          p("", style = "padding-top:10px;")
                                          
                                   ),
                                   column(6, align = "center",
                                          h4(strong("(this is for example)")),
                                          p("", style = "padding-top:10px;"),
                                          img(src = "faculty-chen.jpg", style = "display: inline; border: 0px solid #C0C0C0;", width = "150px"),
                                          img(src = "faculty-gupta.jpg", style = "display: inline;  border: 0px solid #C0C0C0;", width = "150px"),
                                          img(src = "faculty-alwang.jpg", style = "display: inline; border: 0px solid #C0C0C0;", width = "150px"),
                                          p("", style = "padding-top:10px;"),
                                          p(a(href = "https://aaec.vt.edu/people/faculty/chen-susan.html", 'Dr. Susan Chen', target = '_blank'), "(Virginia Tech, Agricultural and Applied Microeconomics);"),
                                          p(a(href = "https://aaec.vt.edu/people/faculty/gupta-anubhab.html", 'Dr. Anubhab Gupta', target = '_blank'), "(Virginia Tech, Agricultural and Applied Microeconomics);"),
                                          p(a(href = "https://aaec.vt.edu/people/faculty/alwang-jeffrey.html", 'Dr. Jeffrey Alwang', target = '_blank'), "(Virginia Tech, Agricultural and Applied Microeconomics)."),
                                          p("", style = "padding-top:10px;")
                                   )
                          ),
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   h4(strong("(this is for example)"), align = "center"),
                                   p(a(href="https://www.linkedin.com/in/dhiraj-sharma-aa029024/?originalSubdomain=np","Dhiraj Sharma",target='_blank')," (World Bank); "),
                                   p("Grown Chirongwe",a(href="https://www.zimstat.co.zw/","(Zimbabwe National Statistics Agency)",target="_blank")),
                                   
                                   p(em("Disclaimer: "),("This project is an academic exercise conducted by VT-Data Science for the Public Good. The findings, interpretations, and conclusions expressed here do not necessarily reflect the views of the World Bank or the Zimbabwe Statistical Agency."))
                                   
                          ),
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   h4(strong("(this is for example)"), align = "center"),
                                   p("We would like to thank ",a(href="https://www.linkedin.com/in/quentin-stoeffler-7913a035/?originalSubdomain=tr","Dr. Quentin Stoeffler",target='_blank')," for providing us with code of the paper", a(href="https://www.researchgate.net/profile/Jeffrey-Alwang/publication/283241726_Multidimensional_Poverty_in_Crisis_Lessons_from_Zimbabwe/links/56b8978a08ae44bb330d32f2/Multidimensional-Poverty-in-Crisis-Lessons-from-Zimbabwe.pdf","Multidimensional Poverty in Crisis: Lessons from Zimbabwe",target='_blank'),". We also thank ZimStat for providing 2011 and 2017 PICES data for this project.")
                                   
                          )
                 ),
                 ## Tab precipitation------------------------------------------------
                 tabPanel("Precipitation", 
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   h1(strong("Insert Precipitation charts"), align = "center"),
                                   br(),
                                   h4(strong("VT Data Science for the Public Good (this is for example)"), align = "center"),
                                   p("The", a(href = 'https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html', 'Data Science for the Public Good (DSPG) Young Scholars program', target = "_blank"),
                                     "is a summer immersive program offered by the", a(href = 'https://aaec.vt.edu/index.html', 'Virginia Tech Department of Agricultural and Applied Economics'), 
                                     "In its second year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges 
                                               around critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to 
                                               determine how information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program highlights, 
                                               how to apply, and our annual symposium, please visit", 
                                     a(href = 'https://aaec.vt.edu/content/aaec_vt_edu/en/academics/undergraduate/beyond-classroom/dspg.html#select=1.html', 'the official VT DSPG website.', target = "_blank")),
                                   p("", style = "padding-top:10px;")
                          ),
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   column(6, align = "center",
                                          h4(strong("(this is for example)")),
                                          p("", style = "padding-top:10px;"),
                                          img(src = "team-yang.png", style = "display: inline;  border: 0px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-sambath.jpg", style = "display: inline; border: 0px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-atticus.jpg", style = "display: inline; border: 0px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-matt.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p("", style = "padding-top:10px;"),
                                          p(a(href = 'https://www.linkedin.com/in/yang-cheng-200118191/', 'Yang Cheng', target = '_blank'), "(Virginia Tech, Agricultural and Applied Microeconomics);"),
                                          p(a(href = 'https://www.linkedin.com/in/sambath-jayapregasham-097803127/', 'Sambath Jayapregasham', target = '_blank'), "(Virginia Tech, Agricultural and Applied Microeconomics);"),
                                          p(a(href = 'https://www.linkedin.com/in/atticus-rex-717581191/', 'Atticus Rex', target = '_blank'), "(Virginia Tech, Computational Modeling and Data Analytics);"),
                                          p( a(href = 'https://www.linkedin.com/in/matthew-burkholder-297b9119a/', 'Matthew Burkholder', target = '_blank'), "(Virginia Tech, Philosophy, Politics, & Economics)."),
                                          p("", style = "padding-top:10px;")
                                          
                                   ),
                                   column(6, align = "center",
                                          h4(strong("(this is for example)")),
                                          p("", style = "padding-top:10px;"),
                                          img(src = "faculty-chen.jpg", style = "display: inline; border: 0px solid #C0C0C0;", width = "150px"),
                                          img(src = "faculty-gupta.jpg", style = "display: inline;  border: 0px solid #C0C0C0;", width = "150px"),
                                          img(src = "faculty-alwang.jpg", style = "display: inline; border: 0px solid #C0C0C0;", width = "150px"),
                                          p("", style = "padding-top:10px;"),
                                          p(a(href = "https://aaec.vt.edu/people/faculty/chen-susan.html", 'Dr. Susan Chen', target = '_blank'), "(Virginia Tech, Agricultural and Applied Microeconomics);"),
                                          p(a(href = "https://aaec.vt.edu/people/faculty/gupta-anubhab.html", 'Dr. Anubhab Gupta', target = '_blank'), "(Virginia Tech, Agricultural and Applied Microeconomics);"),
                                          p(a(href = "https://aaec.vt.edu/people/faculty/alwang-jeffrey.html", 'Dr. Jeffrey Alwang', target = '_blank'), "(Virginia Tech, Agricultural and Applied Microeconomics)."),
                                          p("", style = "padding-top:10px;")
                                   )
                          ),
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   h4(strong("(this is for example)"), align = "center"),
                                   p(a(href="https://www.linkedin.com/in/dhiraj-sharma-aa029024/?originalSubdomain=np","Dhiraj Sharma",target='_blank')," (World Bank); "),
                                   p("Grown Chirongwe",a(href="https://www.zimstat.co.zw/","(Zimbabwe National Statistics Agency)",target="_blank")),
                                   
                                   p(em("Disclaimer: "),("This project is an academic exercise conducted by VT-Data Science for the Public Good. The findings, interpretations, and conclusions expressed here do not necessarily reflect the views of the World Bank or the Zimbabwe Statistical Agency."))
                                   
                          ),
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   h4(strong("(this is for example)"), align = "center"),
                                   p("We would like to thank ",a(href="https://www.linkedin.com/in/quentin-stoeffler-7913a035/?originalSubdomain=tr","Dr. Quentin Stoeffler",target='_blank')," for providing us with code of the paper", a(href="https://www.researchgate.net/profile/Jeffrey-Alwang/publication/283241726_Multidimensional_Poverty_in_Crisis_Lessons_from_Zimbabwe/links/56b8978a08ae44bb330d32f2/Multidimensional-Poverty-in-Crisis-Lessons-from-Zimbabwe.pdf","Multidimensional Poverty in Crisis: Lessons from Zimbabwe",target='_blank'),". We also thank ZimStat for providing 2011 and 2017 PICES data for this project.")
                                   
                          )
                 ),
                 inverse = T)


server <- function(input, output, session){
    
    output$MapGraph <- renderLeaflet({
        mypal <- colorNumeric(
            palette = "viridis",
            domain = NULL,
            reverse = TRUE)
        
        leaflet(MapDataFin) %>% addTiles() %>%
            addPolygons(color = ~mypal(MapDataFin$AvgSurfaceMoisture), weight = 1, smoothFactor = 0.5, label = paste("Region ", MapDataFin$region, ":", round(MapDataFin$AvgSurfaceMoisture, digits = 3)),
                        opacity = 1.0, fillOpacity = 0.5,
                        highlightOptions = highlightOptions(color = "black", weight = 2,
                                                            bringToFront = TRUE)) %>%
            addPolylines(data = MapDataFin$geometry, color = "black", opacity = 2, weight = 2) %>% 
            addLegend(pal = mypal,position = "bottomleft",values = MapDataFin$AvgSurfaceMoisture, opacity = .6,
                      title= paste("Average Soil Moisture During The First 30 days Of 2016-17 Growing Season"))
    })       
    output$BarGraph <- renderPlot({
        ggplot(BarData, aes(fill=time, y=value, x=region)) + 
            geom_bar(position="dodge", stat="identity")+ 
            labs(color="time") +
            xlab("Agro-ecological Region") + ylab("Number Of 3-Day periods") + 
            ggtitle("Soil Moisture Conditions In The Planting Time During The 2016-17 Growing Season") +
            guides(fill=guide_legend(title="Soil Condition")) + labs(caption = "3 Day: NASA-USDA Enhanced SMAP Global") +
            scale_fill_viridis(discrete=TRUE, direction=-1)
        #3day periods within 30 days of 11/19/16 by region and Surf-soil moisture condition
    })
    
    output$LineGraph <- renderPlot({
        
        ggplot(LineData, aes(as.Date(newDate), y = value, color = variable)) + 
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
        
    })
    
    
}








shinyApp(ui, server)
