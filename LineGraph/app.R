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
library(readxl)
library(dplyr)
library(tidyverse)
library(sf)
library(ggplot2)
library(rgdal)
library(sp)
library(directlabels)
library(ggrepel)
library(plotly)

GrowSs2011 <- read_csv("./Data/EVIGrow2011.csv")
GrSs2011Line <- read_csv("./Data/GrSs2011Line.csv")



ui <- fluidPage(
  selectInput(inputId = "Region", 
              label = "Choose a Region", 
              choices = unique(GrSs2011Line$Region)), 
  # Application title
  titlePanel("Frankie's graphs"),
  sidebarLayout(
    sidebarPanel(p("Demo")),
    mainPanel(strong("EVI in Growing Season 2011"),
              plotlyOutput("plot1")
              
    )    
  )       
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$plot1 <- renderPlotly({
    
    GrSs2011Line$Month[which(GrSs2011Line$Month=="10")] <- "October"
    GrSs2011Line$Month[which(GrSs2011Line$Month=="11")] <- "November"
    GrSs2011Line$Month[which(GrSs2011Line$Month=="12")] <- "December"
    GrSs2011Line$Month[which(GrSs2011Line$Month=="01")] <- "January"
    GrSs2011Line$Month[which(GrSs2011Line$Month=="02")] <- "Febuary"
    GrSs2011Line$Month[which(GrSs2011Line$Month=="03")] <- "March"
    GrSs2011Line$Month[which(GrSs2011Line$Month=="04")] <- "April"
    GrSs2011Line$Month[which(GrSs2011Line$Month=="05")] <- "May"
    
    GrSs2011Line$Month <- reorder(GrSs2011Line$Month, as.numeric(GrSs2011Line$GSOrder))
    GrSs2011Line$Month <- as.factor(GrSs2011Line$Month)
    GrSs2011Line$Region <- as.factor(GrSs2011Line$Region)
    
    ## if else statement in R, assigning colors manually to each region for the line graph  
    
    if (input$Region == 'I') {
      color <- "red"
    }else if (input$Region == 'IIA'){
      color <- "green"
    }
    
    GrSs2011Line %>% 
      filter(Region == input$Region) %>% 
      ggplot(aes(x = Month, y = MaxEVI, group = as.factor(Region), color = as.factor(Region))) +
      geom_line(aes(color = color))+
      theme(axis.text.x = element_text(angle = 315)) +
      scale_colour_discrete(guide = 'none') +
      scale_x_discrete(expand=c(0, 1)) +
      geom_dl(aes(label = Region), method = list(dl.combine("first.points", "last.points")), cex = 0.8) +
      # scale_color_viridis_d(option = "H") +
      labs(title = "Max EVI in Zim During Growing Season 2011", color =  "Region") +
      xlab("Time(Month)") +
      ylab("Max EVI")+ 
      plotly()
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
