library(shiny)
library(leaflet)
library(readr)
library(sf)
library(shinythemes)
library(markdown)
library(slickR)


my_images <- c("av_dec_compared_up.jpg","av_jan_compared_up.jpg","av_feb_compared_up.jpg")
my_images2 <- c("Precip Reg_Table 1.png","Precip Reg_Table 2.png","Precip Reg_Table 3.png","Precip Reg_Table 4.png")
my_images3 <- c("EVI Reg_Table 1.png","EVI Reg_Table 2.png")
my_images4 <- c("Soil Reg_Table 1.png","Soil Reg_Table 2.png")

# Define UI for application that draws a histogram
ui <- navbarPage(
  theme = shinytheme('lumen'), 
  title = "Zimbabwe Project: Remote Sensed Data",
  tabPanel(
    strong("Precipitation"),
    fluidRow(
      style = "margin-left: 100px; margin-right: 100px;",
      h1(strong("Precipitation Index (TRMM 3B42)"), 
         style = "font-size:35px;"),
      align = "center"
    ),
    fluidRow(
      style = "margin-left: 100px; margin-right: 100px;",
      p(strong("3-month observation 2011-17"))
      ),
    fluidRow(
      style = "margin-left: 0px; margin-right: 0px;",
      column(12, slickROutput("my_slick"), offset = 0, br(),br(),br())
      
    ),
    fluidRow(
      style = "margin-left: 100px; margin-right: 100px;",
      p("These maps are looking at rain distribution at average daily level. Zimbabwe generally follows previous analysis of  its weather pattern but as it relates to precipitation the Northern regions are typically the ones to receive the most rainfall. 
        The Southern region on the other hand receives less rainfall (Nkomozepi & Chung, 2012). In comparison, 2010-11 vs 2016-17 we are seeing slightly more precipitation in 2016-17 growing period on a seasonal level and month by month level. This could possibly indicate a higher yield for maize. 
        A day is considered wet if it exceeds 2.95 mm of precipitation. ")
    ),
    fluidRow(
      style = "margin-left: 100px; margin-right: 100px;",
      h1(strong("Comparison of Total Rainfall between 2010 & 2016 Growing Season"), 
         style ="font-size: 25px;"),
      align = "left"
      ),
    fluidRow(
      style = "margin-left: 5px; margin-right: 100px;",
      img(src = "totalrainfall_compared.jpg"),
      align = "left"
    ),
    fluidRow(
      style = "margin-left: 100px; margin-right: 100px;",
      column(12, p(strong("Description")) , 
             p("This is showing us the total rainfall in the two growing seasons. 
             For maize production, the ideal range of rainfall is 600-700 mm. 2010-11 Growing Season:
            Regions I, IIA, IIB, & III at the North-East are observed as receiving more rainfall than its South-Western counterparts. 
            However, It should be noted that Region IV still received the minimum rainfall for an average yield of maize.2016-17 Growing Season: 
            All regions minus region v have received a healthy range of rainfall within the 2016-17 growing season. 
            It should be noted that in region I, exceeding the 1000 mm threshold indicates that the maize yield may have declined for this region. 
            In the same light regions IIA,IIB, & III received up to 1000 mm of rainfall which may have led to an increase in the maize yield.
"))
      
    ),
    fluidRow(
      style = "margin-left: 100px; margin-right: 100px;",
      h1(strong("Comparison of Dryspells between 2010 & 2016 Growing Season"),
         style = "font-size: 25px;"),
      align = "left"
    ),
    fluidRow(
      style = "margin-left: 70px; margin-right: 100px;",
      column(12, img(src = "dry_compared.jpg", height = "100%", width = "100%")),
      align = "center"
    ),
    
    fluidRow(
      style = "margin-left: 100px; margin-right: 100px;",
      column(12, p(strong("Description")) ,
             p("This is showing the number of dry spells; A dry spell is described to be a consecutive series of dry days between 10 to 20 days or 20 days and more. In literature, we found that dry spells play a significant role in agricultural success. The degree and frequency of dry spells before or during can indicate reduction of a growing season or flat out crop failure. 
2010-11
In a given month within this growing season all regions experienced multiple dry spells that lasted more than 20 days. 
2016-17
In a given month within this growing season all regions experienced one if not multiple dry spells that lasted more than 20 days. The growing season has experienced a lower frequency of dry spells but they seem to have a wider spread of dry spells that were 20 days and more among all the regions.
"))
      )
    
    ),
  navbarMenu(strong("MPI and Indices"),
             tabPanel(strong("MPI & Precipitation"),
                      fluidRow(
                        style = "margin-left: 0px; margin-right: 0px;",
                        column(8, slickROutput("my_slick2")),
                        column(4, 
                               p(
                                 "Table 1:
                      This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES, 
                      the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:"
                               ),
                               p(
                                 "Table 2:
                        This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES
                        , the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:"
                               ),
                               p(
                                 "Table 3:
                        This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES
                        , the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:"
                               ),
                               p(
                                 "Table 4:
                        This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES
                        , the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:"
                               )
                        )
                        
                      )
                      
             ),
             tabPanel(strong("MPI & EVI"),
                      fluidRow(
                        style = "margin-left: 0px; margin-right: 0px;",
                        column(8, slickROutput("my_slick3")),
                        column(4, 
                               p(
                                 "Table 1:
                      This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES, 
                      the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:"
                               ),
                               p(
                                 "Table 2:
                        This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES
                        , the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:"
                               )
                        )
                        
                      )
                      
             ),
             tabPanel(strong("MPI & Soil Moisture"),
                      fluidRow(
                        style = "margin-left: 0px; margin-right: 0px;",
                        column(8, slickROutput("my_slick4")),
                        column(4, 
                               p(
                                 "Table 1:
                      This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES, 
                      the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:"
                               ),
                               p(
                                 "Table 2:
                        This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES
                        , the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:"
                               )
                        )
                        
                      )
                      
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
  output$my_slick2 <- renderSlickR(
    slickR(
      my_images2,
      width = "90%"
    )
  )
  output$my_slick3 <- renderSlickR(
    slickR(
      my_images3,
      width = "90%"
    )
  )
  output$my_slick4 <- renderSlickR(
    slickR(
      my_images4,
      width = "90%"
    )
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
