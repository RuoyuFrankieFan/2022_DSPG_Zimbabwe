library(shiny)
library(leaflet)
library(readr)
library(sf)
library(shinythemes)
library(markdown)
library(slickR)


my_images <- c("av_dec_2010.jpeg","av_jan_2011.jpeg","av_feb_2011.jpeg",
               "av_dec_2016.jpeg","av_jan_2017.jpeg","av_feb_2017.jpeg")
# Define UI for application that draws a histogram
ui <- navbarPage(
  theme = shinytheme('lumen'), 
  title = "Zimbabwe Project: Remote Sensed Data",
  tabPanel(
    strong("Precipitation"),
    fluidRow(
      h1(strong("Precipitation Index (TRMM 3B42)"), 
         style = "font-size:35px;"),
      align = "center"
    ),
    fluidRow(
      p(strong("Leaflet: 3-month observation 2011-17"))
      ),
    fluidRow(
      column(2),
      column(8, slickROutput("my_slick"), offset = 0, br(),br(),br()),
      align = "center"
      
    ),
    fluidRow(
      p("Although the overview of this map is difficult to understand, context will help with that. This map is showing Average Daily Precipitation in December 2010. So given that the map is looking at rain distribution at a daily level most regions we’re seeing similar amounts of rainfall or a lack of variability per region. A day is considered wet if it exceeds 2.95 mm of precipitation based this we can infer that all regions on average were consider to have wet days.
                                                                  This map is showing Average Daily Precipitation (mm) in January 2011. In contrast from December 2010, We start see higher levels of rainfall which lines up the what literature describes to be the wet season Nov – May. We see a concentration of rainfall in North-East regions
                                                                  This is showing that most regions are seeing about 0-2mm of rainfall in February 2011 mentioned in the literature; a day is consider to be dry if the value is less than 2.95 mm. Indicating low levels rain except in region IIA where they’re receive a good amount of rain.
                                                                  This is showing that most regions are seeing about 2-8mm of rainfall in December 2016 according to the literature these region are receiving healthy levels of rainfall and this month is considered to have on average number of wet days among the regions.
                                                                  This is showing that most regions are seeing about 6-12mm of rainfall in January 2017. Generally, this is lining up with what literature considers to be the wet season.
                                                                  This is showing that most regions are seeing about 4-8mm of rainfall in February 2017. Generally, this is lining up with what literature considers to be the wet season.
                                                                  In comparison, 2010-11 vs 2016-17 we are seeing slightly more precipitation in 2016-17 growing period on a seasonal level and month by month level. This could possibly indicate a higher yield for maize.")
    ),
    fluidRow(
      h1(strong("Comparison of Total Rainfall between 2010 & 2016 Growing Season"), 
         style ="font-size: 25px;"),
      align = "center"
      ),
    fluidRow(
      column(6, img(src = "Totalrainfall_2011.jpeg")), 
      column(6, img(src = "Totalrainfall_2017.jpeg"))
    ),
    fluidRow(
      column(12, p(strong("Description")) , 
             p("This is showing us the total rainfall in the 2010-11 growing season. For Maize production, the ideal range of rainfall is 600-700 mm. Regions I,IIA,IIB, & III at the North-East are observed as receiving more rainfall than its South-Western counterparts. However, It should be noted that Region IV still received the minimum rainfall for an average yield of Maize.This is showing that in all regions minus region v have received a health range of rainfall within the 2016-17 growing season. It should be noted that in region I, exceeded 1000 mm threshold that indicates the maize yield may have declined for this region. In the same light regions IIA,IIB, & III received up towards 1000 mm of rainfall which may have lead to an increase in the maize yield."))
      
    ),
    fluidRow(
      h1(strong("Comparison of Dryspells between 2010 & 2016 Growing Season"),
         style = "font-size: 25px;"),
      align = "center"
    )
    ),
    fluidRow(
      column(5, img(src = "numDry10.jpeg")), 
      column(6, img(src = "numDry16.jpeg")),
      align = "center"
    ),
    fluidRow(
      column(12, p(strong("Description")) ,
             p("This is showing the count of dry spells; A dry spell is described to be a consecutive series of dry days between 10 to 20 days or 20 days and more. In a given month within this growing season all regions experienced multiple dry spells that lasted more than 20 days. In literature, we have found that dry spells play a significant role on agricultural success. The degree and frequency of dry spells before or during can indicate reduction a growing season or flat out crop failure.
               This is showing the count of dry spells; A dry spell is described to be a consecutive series of dry days between 10 to 20 days or 20 days and more. In a given month within this growing season all regions experienced one if not multiple dry spells that lasted more than 20 days. In literature, we have found that dry spells play a significant role on agricultural success. The degree and frequency of dry spells before or during can indicate reduction of a growing season or flat out crop failure. In contrast, 2016-18 growing season has experience a lower frequency of dry spells but they seem to have more wide spread of dry spells that were 20 days and more among all the regions.
               ")
    )
    
)
)



# Define server logic required
server <- function(input, output) {
  output$my_slick <- renderSlickR(
    slickR(
      my_images,
      width = "90%"
    )
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
