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
             p("
               These visualizations show us the total rainfall during the two growing seasons,  
               which when compared indicate that every region received a higher amount of rainfall in 2016-17 than in 2010-11. 
               This is consistent  with past Literature, which indicates that Zimbabwe experienced a severe drought during 2015-2016. 
               Rainfall patterns are also consistent, as North-East regions typically receive more rainfall than their South-Western counterparts (Nkomozepi & Chung, 2012). 
               For maize production, the ideal range of rainfall is 600-700 mm with excess of 1000 mm potentially leading  to a decline in maize yields. 
               This means that in both growing seasons, All regions except for V met the minimum amount of rain necessary for an average maize yield, 
               with region 1 having an excess of rain in growing season 2016-17 that may lead to decreases in yield.   
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
             p("
               This shows the number of dry spells; A dry spell is described to be a 
               consecutive series of dry days between 10 to 20 days or 20 days or more. 
               Dry spells play a significant role in agricultural success by indicating 
               plant stress exposure while their length and severity can result in the decreased 
               yields or complete failure of a crop (Nyakudya et al., 2011;Mhizha et al., 2014).  
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
