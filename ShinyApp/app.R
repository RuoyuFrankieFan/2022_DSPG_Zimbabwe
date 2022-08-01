#Load Libraries
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
library(viridis)
library(zoo)
library(stringr)
library(sp)
library(directlabels)
library(slickR)
#gpclibPermit()

## FORMATTING-------------------------------------------------------------------
prettyblue <- "#232D4B"
navBarBlue <- '#427EDC'
options(spinner.color = prettyblue, spinner.color.background = '#ffffff', spinner.size = 3, spinner.type = 1)

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

# CODE TO DETECT ORIGIN OF LINK AND CHANGE LOGO ACCORDINGLY------
jscode <- "function getUrlVars() {
                var vars = {};
                var parts = window.location.href.replace(/[?&]+([^=&]+)=([^&]*)/gi, function(m,key,value) {
                    vars[key] = value;
                });
                return vars;
            }

           function getUrlParam(parameter, defaultvalue){
                var urlparameter = defaultvalue;
                if(window.location.href.indexOf(parameter) > -1){
                    urlparameter = getUrlVars()[parameter];
                    }
                return urlparameter;
            }

            var mytype = getUrlParam('type','Empty');

            function changeLinks(parameter) {
                links = document.getElementsByTagName(\"a\");

                for(var i = 0; i < links.length; i++) {
                   var link = links[i];
                   var newurl = link.href + '?type=' + parameter;
                   link.setAttribute('href', newurl);
                 }
            }

           var x = document.getElementsByClassName('navbar-brand');

           if (mytype != 'economic') {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/\">' +
                              '<img src=\"DSPG_black-01.png\", alt=\"DSPG 2022 Symposium Proceedings\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('dspg');
           } else {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/economic-mobility/community-insights/case-studies\">' +
                              '<img src=\"AEMLogoGatesColorsBlack-11.png\", alt=\"Gates Economic Mobility Case Studies\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('economic'); 
           }
           "


# LOADING DATA-----------------------------------------------
#SHAPEFILES
zim_district <- st_read("./data/shapefiles/Zim_D60.shp")  
zim_region <- st_read("./data/shapefiles/agro-ecological-regions.shp")

zim_region <- rename(zim_region, Region = "nat_region")

#Map palette
mypal <- colorNumeric(
  palette = "viridis",
  domain = NULL,
  reverse = TRUE)


#EVI DATA
#agregion
GrSs2011 <- read_csv("./data/agregion/evi/EVI_region_GrSs2011.csv")
GrSs2017 <- read_csv("./data/agregion/evi/EVI_region_GrSs2017.csv")
#GrSs2011 <- rename(GrSs2011, region="Region")
#GrSs2017 <- rename(GrSs2017, region="Region")

EVI_region_long <- read_csv("./data/agregion/evi/EVI_region_long.csv")

GrSs2011Line <- read.csv("./data/agregion/evi/eviline2011.csv")
GrSs2017Line <- read.csv("./data/agregion/evi/eviline2017.csv")
#GrSs2011Line <- rename(GrSs2011Line, region="Region")
#GrSs2017Line <- rename(GrSs2017Line, region="Region")


EVIGrow2011 <- full_join(zim_region, GrSs2011, by = "Region")
EVIGrow2017 <- full_join(zim_region, GrSs2017, by = "Region")

my_images_evi <- c("Max EVI 2011.png", "Max EVI 2017.png")


#PRECIPITATION DATA
my_images <- c("av_dec_compared_up.png","av_jan_compared_up.png","av_feb_compared_up.png")




#SOIL DATA
SurfMapDataPre <- read.csv("data/agregion/soil/SoilMapPlotData.csv")
SurfBarData <- read.csv("data/agregion/soil/SoilBarPlotData.csv")
SurfLineData <- read.csv("data/agregion/soil/SoilLinePlotData.csv")
SurfMapDataPre <-rename(SurfMapDataPre, Region = "region")

PercMapDataPre <- read.csv("data/agregion/soil/PercSoilMapPlotData.csv")
PercBarData <- read.csv("data/agregion/soil/PercSoilBarPlotData.csv")
PercLineData <- read.csv("data/agregion/soil/PercSoilLinePlotData.csv")
PercMapDataPre <-rename(PercMapDataPre, Region = "region")


#zim_region <- st_read("data/shapefiles/agro-ecological-regions.shp")
#zim_region <-rename(zim_region, region = nat_region)

SurfMapDataTwo <- list(zim_region, SurfMapDataPre)
SurfMapDataFin <- SurfMapDataTwo %>% reduce(full_join, by='Region')

PercMapDataTwo <- list(zim_region, PercMapDataPre)
PercMapDataFin <- PercMapDataTwo %>% reduce(full_join, by='Region')

# Set axis limits c(min, max) on plot
Surfmin <- as.yearmon("20161119", "%Y%m")
Surfmax <- as.yearmon("20161219", "%Y%m")
Surfmin <- as.Date("2016-11-19")
Surfmax <- as.Date("2016-12-19")

# Set axis limits c(min, max) on plot
Percmin <- as.yearmon("20161219", "%Y%m")
Percmax <- as.yearmon("20170529", "%Y%m")
Percmin <- as.Date("2016-12-19")
Percmax <- as.Date("2017-05-29")



# MapDataPre <- read.csv("data/agregion/soil/SoilMapPlotData.csv")
# BarData <- read.csv("data/agregion/soil/SoilBarPlotData.csv")
# LineData <- read.csv("data/agregion/soil/SoilLinePlotData.csv")
# MapDataPre <-rename(MapDataPre, Region = "region")
# 
# #BarData <-rename(BarData, Region = "region")
# #zim_region <- st_read("data/shapefiles/agro-ecological-regions.shp")
# #zim_region <-rename(zim_region, Region = nat_region)
# 
# MapDataTwo <- list(zim_region, MapDataPre)
# MapDataFin <- MapDataTwo %>% reduce(full_join, by='Region')
# 
# # Set axis limits c(min, max) on plot
# min <- as.yearmon("20161119", "%Y%m")
# max <- as.yearmon("20161219", "%Y%m")
# min <- as.Date("2016-11-19")
# max <- as.Date("2016-12-19")



#MPI DATA
MPI <- read.csv("./data/MPI/MPI_Dataset.csv")
MPI_2011 <- MPI[which(MPI$year=="2011"),]
MPI_2017 <- MPI[which(MPI$year=="2017"),]
MPI_2011 <- rename(MPI_2011, District_ID="district_id")
MPI_2017 <- rename(MPI_2017, District_ID="district_id")

#MPI_2011 <- read_excel("./data/MPI/2011_MPI_w_components.xlsx")
#MPI_2017 <- read_excel("./data/MPI/2017_MPI_w_components.xlsx")

my_images2 <- c("Precip Reg_Table 1.png","Precip Reg_Table 2.png","Precip Reg_Table 3.png","Precip Reg_Table 4.png")
my_images3 <- c("EVI Reg_Table 1.png","EVI Reg_Table 2.png")
my_images4 <- c("Soil Reg_Table 1.png","Soil Reg_Table 2.png")
my_images5 <- c("Descriptive Statistics - 2011.png", "Descriptive Statistics - 2017.png", "Correlations - 2011.png", "Correlations - 2017.png")
my_images6 <- c("stats_v2_2011.png", "stats_2017.png")
my_images7 <- c("mpi_precip1.png","mpi_precip2.png","mpi_precip3.png","mpi_precip4.png"
                ,"mpi_precip5.png","mpi_precip6.png","mpi_precip7.png")
my_images8 <- c("mpi_rural_precip1.png","mpi_rural_precip2.png","mpi_rural_precip3.png")
my_images9 <- c("mpiadj_precip1.png","mpiadj_precip2.png","mpiadj_precip3.png") 





##Join data
zim_district <- rename(zim_district, District_name = "NAME_2")
zim_district <- zim_district %>% 
  arrange(District_name) %>% 
  mutate(District_ID = c(1:60))

joined_zim <-  full_join(zim_district, MPI_2011, by = "District_ID")
joined_zim17 <-  full_join(zim_district, MPI_2017, by = "District_ID")





# UI -------------------------------------------------------------

ui <- navbarPage(title = "Zimbabwe",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                 useShinyjs(),
                 #                  tags$script(HTML("var header = $('.navbar > .Zimbabwe');
                 # header.append('<div style=\"float:right\"><ahref=\"URL\"><img src=\"github.png\" alt=\"alt\" style=\"float:right;width:33px;height:41px;padding-top:10px;\"> </a>`</div>');
                 #     console.log(header)")
                 #                  ),
                 ## Tab Overview -----------------------------------------------------------
                 tabPanel(strong("Overview"), value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   br(""),
                                   h1(strong("Using Remote Sensed Data For Social & Economic Decision Making In Zimbabwe")),
                                    fluidRow(style = "margin: 2px;",
                                             img(src = "corn-field.jpg", height="100", width="800", alt="Image", style="display: block; margin-left: auto; margin-right: auto; border: 1px solid #000000;")),
                                   h4("Data Science for the Public Good Program"),
                                   h4("Virginia Tech"),
                                   h4("Department of Agricultural and Applied Economics")
                                   
                          ),
                          
                          fluidRow(style = "margin: 6px;",
                                   column(6,
                                          align="justify",
                                          h2(strong("Project Overview"), align = "center"),
                                          p("In Zimbabwe, agriculture is a mainstay of the economy and livelihood for most rural poor. Zimbabwe has experienced increased social and economic unrest since 2000, with macroeconomic instability and diseases contributing to the problem. Extreme droughts in 2003 and 2016 contributed to increased food insecurity and a significant increase in rural poverty. Additionally, a fast-track land reform program beginning in 2000 contributed to the decapitalization of the commercial agriculture sector."),
                                          p("In this project, we identify the remotely sensed climate-related data that are publicly available and suitable for Zimbabwe. These are the Enhanced Vegetation Index (EVI), Precipitation, and Soil Moisture datasets. We use these indices to provide a geospatial analysis of the five agro-ecological regions in the 2010-11 and 2016-17 growing seasons. We then analyze the climatic conditions ideal for maize, the primary crop grown in Zimbabwe."),
                                          br(),
                                          fluidRow(
                                            column(12,
                                            img(src = "timeline.png", style = "display: inline; border: 0px solid #C0C0C0; margin-left: auto; margin-right: auto;", width = "100%"), align="center",
                                            div(tags$caption("Figure 1: Project Timeline"), align="center"),
                                          #div(tags$caption("Table 1: Agro-ecological regions in Zimbabwe")),
                                          #withSpinner(tableOutput("table")),
                                          #div(tags$caption("Source: FAO"))
                                          )),
                                          br(),
                                          p("Our analysis is further disaggregated to the district-level to study the association between poverty and climate indicators. To perform this study, we augment the climate data with poverty variables constructed from the national Poverty Income Consumption Expenditure Survey (PICES) conducted in 2011 and 2017."),
                                          p("Finally, we use these data in a statistical model to examine the association between district-level poverty and climatic conditions. The results of our analysis provide a spatially disaggregated look at whether climate data can be used to identify at-risk regions for potential policy intervention. This is useful because while the Zimbabwean government has recently approved an agricultural policy framework based on climate-smart principles, it contains little geographic specificity for an incredibly diverse agricultural economy.")),
                                   
                                   
                                   column(6,
                                          align="justify",
                                          h2(strong("Introduction To Zimbabwe"), align = "center"),
                                          p("Zimbabwe is located in the Southeastern part of Africa and neighbors South Africa, Mozambique, Zambia, and Botswana. Zimbabwe gained independence in 1980 and was ruled by former Prime Minister President Robert Mugabe until his resignation in 2017. In the first decade after independence, there were efforts to address poverty. Still, they were ineffective and abandoned due to a financial downturn coupled with a prolonged drought that forced agricultural workers into the cities. In the city, they faced even greater poverty due to unemployment. Efforts to restore the economy led to a budget deficit, and fiscal policy focused on increasing the amount of money in circulation, which resulted in hyperinflation (extremely high prices). Adopting the US dollar stabilized the economy initially, but in 2013 the government shifted efforts, and the overall economic crisis and poverty worsened."), 
                                            p("Zimbabwe has vast amounts of arable land, and about 67.5 percent of the labor force works in agriculture, growing maize, sugar cane, tobacco, fruit, and vegetables. Another 7.3 percent of the labor force takes advantage of Zimbabwe's rich natural resources and participates in mining. Zimbabwe exports coal, gold, platinum, copper, and other metals and manufactures wood products, cement, chemicals, fertilizer, and food. Despite being relatively well-educated and highly literate, the population suffers from unemployment and severe underemployment. Many individuals are either overqualified for their jobs or are not engaging in full-time work. Together with low wages, this creates an obstacle to economic growth."),
                                          
                                          img(src = "SelectedRegion.png", style = "display: inline; border: 0px solid #C0C0C0; margin-left: auto; margin-right: auto;", width = "105%"),
                                          div(tags$caption("Figure 2: Livelihood zones in Zimbabwe"), align="center"),
                                          br(),
                                            p("Currently, President Emmerson Mnangagwa holds office. Zimbabwe is home to roughly 15 million inhabitants, 10% of whom live in the capital city of Harare. Although large clusters exist in other major urban areas, including Bulawayo and Chitungwiza, the population distribution is relatively evenly dispersed throughout the country. Zimbabwe's central government is responsible for regulating its ten provinces and 59 further subdivided districts. Zimbabwe's terrain consists mainly of a flat plateau upon which forests thrive."))),
                                   
                          br(),
                          br(),
                          br(),
                          br(),
                                   
                                   fluidRow(style = "margin: 6px;",
                                   column(6,
                                          align="justify",
                                          h2(strong("Agricultural Profile"), align = "center"),
                                          p("Eighty-nine percent of farmers in Zimbabwe are smallholders, defined as farmers with limited resources that work on small plots of land, have little technology, and typically rely on family for labor and rain for their crops (Kuhudzayi & Mattos, 2018). Less than one percent have access to irrigation (Milne, Mekonnen, & Benitez Ponce, 2019). Limited access to water affects food production and leads to food insecurity issues, especially during droughts exacerbated by irrigation and water storage infrastructure being in a state of disrepair (Milne, Mekonnen, & Benitez Ponce, 2019). Climate change is anticipated to harm agriculture due to more floods, droughts, and changing temperatures and precipitation patterns. Without adequate adaptation, a drier climate is projected to decrease GDP due to agriculture sector losses by 2 percent (Milne, et al., 2019)."),
                                          p("There are five agro-ecological (or natural) regions in Zimbabwe that are separated based on multiple factors, including rainfall patterns, vegetation, temperature, and soil quality (ZIMSTAT, 2017). While the government of Zimbabwe and this project uses the official Agro-ecological Regions, there is government interest in updating the map of the regions since they no longer accurately reflect current realities due to social and biophysical environment changes (Milne et al., 2019). Such changes include climate projections predicting a hotter and drier Zimbabwe, less predictable rainfall, a shorter growing period, soil and ground cover loss, changes to land use, and a decrease in runoff (Milne et al., 2019)."),
                                          
                                          #h3(em("Regional Specificity"), align = "center"),
                                          img(src = "AgroRegionZim.png", style = "display: inline; border: 0px solid #C0C0C0; margin-left: auto; margin-right: auto;", width = "90%"),
                                          div(tags$caption("Figure 3: Agro-ecological Regions Of Zimbabwe"), align="center"),
                                          p("")
                                          #p("Depending on the region, certain crops are more suited than others with regions I, II, and III being better suited to producing commercial crops due to their better rainfall patterns, while regions IV and V are better suited to livestock farming and irrigated agriculture (Milne, Mekonnen, & Benitez Ponce, 2019). To be more specific, Region I is suitable for fruit, forestry, and intensive livestock production; Region II can grow maize, cotton, flue cured tobacco, sugar, beans, and coffee and grows sorghum, seed maize, barley, groundnuts, and various horticultural crops as well; Region III is mostly used for extensive beef ranching and commercial farm production is primarily consisted of Maize; while regions IV and V require irrigation for successful crop production due to their dryness, communal farmers must grow crops without access to irrigation anyway, with millet and sorghum being the most common crops and maize being grown as well"),
                                          
                                          ),

                                   
                                   column(align="justify",
                                     6,
                                     fluidRow(
                                     div(tags$caption("Table 1: Agro-ecological Regions In Zimbabwe")),align="left",
                                     withSpinner(tableOutput("table")),
                                     div(tags$caption("Source: FAO"))),
                                     br(),
                                     
                                     p("Depending on the region, certain crops are more suited than others, with regions I, II, and III being better suited to producing commercial crops due to their better rainfall patterns. In contrast, regions IV and V are better suited to livestock farming and irrigated agriculture (Milne, Mekonnen, & Benitez Ponce, 2019). Region I is suitable for fruit, forestry, and intensive livestock production. Region II can grow maize, cotton, flue-cured tobacco, sugar, beans, coffee, sorghum, seed maize, barley, groundnuts, and horticultural crops. Region III is suitable for beef ranching. Commercial farm production is primarily maize. Regions IV and V require irrigation for successful crop production due to their dryness. Communal farmers must grow crops without access to irrigation anyway, with millet and sorghum being the most common crops but some maize being produced as well")
                                   )
                                   
                                   
                                   
                                   
                                   
                                          ),
                          # fluidRow(
                          #   column(
                          #     align="center",
                          #     6,
                          #     div(tags$caption("Table 1: Agro-ecological regions in Zimbabwe")),align="left",
                          #     withSpinner(tableOutput("table")),
                          #     div(tags$caption("Source: FAO")),
                          #   )
                          # ),
                          br(),
                          br(),

                          
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: August 2022'))))
                 ),
                 
                 ## Tab Data & Methodology-----------------------
                 tabPanel(strong("Data & Methodology"),
                          tabsetPanel(
                            tabPanel(strong("Data"),
                                    
                                     fluidRow(
                                       column(
                                         h1(strong("Remote Sensed Data")),
                                         align="justify",
                                         width = 6,
                                         withMathJax(),
                                         title = h1(strong("Remote Sensed Data")),
                                         p("Remote sensing is the process of getting information from a distance. Our remotely sensed datasets are sourced from NASA, which observes Earth's reflected or emitted energy through sensors on aircraft or satellites (NASA, 2019). In this project, we sourced data from Google Earth Engine (GEE) for the following remotely sensed data:"),
                                         
                                         fluidRow(
                                         img(src = "remotesense.gif", style = "display: inline; border: 0px solid #C0C0C0; margin-left: auto; margin-right: auto;", width = "35%"), align ="center",
                                         div(tags$caption("Figure 4: Remote Sensing"),align="center"),
                                         p(tags$small(em('Source: NASA')))),
                                         #br(),
                                         #p("In this project we source data for the following remote sensed data:"),
                                         #h4(em("Selected Remote Sensed Data")),
                                         withMathJax(),
                                         h3(strong("Enhanced Vegetation Index (EVI)")),
                                         p("Enhanced Vegetation Index (EVI) can quantify vegetation greenness and provides a scope to look at vegetation states and processes (NASA, 2019). Compared to the other index derived from the Moderate Resolution Imaging Spectroradiometer (MODIS), the normalized difference vegetation index (NDVI), EVI minimizes canopy-soil variations and improves sensitivity over high biomass regions (Didan et al., 2022). While NDVI is sensitive to chlorophyll, EVI is more responsive to structural variation in the canopy. This increased responsiveness is helpful in vegetation monitoring because 70% of Earth's terrestrial surface is made up of open canopies whose background signals can distort reflectance observations (Huete et al., 2002). EVI was developed to optimize the vegetation signal by reducing atmospheric influences and decoupling the canopy background signal. This improves sensitivity when monitoring vegetation in high biomass areas (Huete et al., 2002)."),
                                         #br(),
                                         #fluidRow(
                                         #img(src = "evi_zim.png", style = "display: inline; border: 0px solid #C0C0C0; margin-left: auto; margin-right: auto;", width = "40%"), align ="center",
                                         #div(tags$caption("Figure 3: EVI of Zimbabwe"),align="center"),
                                         #),
                                         br(),
                                         withMathJax(),
                                         p("The MODIS Terra Daily EVI dataset is one of two vegetation indices produced from reflectance in the red, near-infrared, and blue wavebands, which is retrieved from the MODIS sensor aboard the Terra Satellite (Didan, Maccherone, & Frazier). The surface spectral reflectance data are corrected for atmospheric conditions like gasses, aerosols, and Rayleigh scattering (Vermote, 2015)."),
                                         
                                         h3(strong("Precipitation")),
                                         p("Tropical Rainfall Measuring Mission (TRMM) 3B42 is a Google Earth Engine (GEE) index containing observations on all forms of tropical precipitation such as snow, rain, and drizzle. The dataset is provided by NASA Goddard Earth Sciences Data and Information Services Center (GES DISC) at NASA Goddard Space Flight Center. It has undergone processing through their TMPA (TRMM Multi-satellite Precipitation Analysis) Algorithm, in which merged high quality (HQ)/infrared (IR) precipitation and root-mean-square (RMS) precipitation-error estimates result in a dataset. The data is collected in 3-hour periods and rendered at a resolution of 27830 meters (about 17.29 mi) observed around the global belt (50° North and South). The unit of measurement provided is in millimeters per hour (TRMM, 2012)."),
                                         
                                         h3(strong("Soil Moisture")),
                                         p("Our dataset, the NASA-USDA Enhanced Soil Moisture Active Passive (SMAP) Global soil moisture data, provides global soil moisture information at a 10 km spatial resolution and includes five indices: Surface and Subsurface soil moisture, Soil moisture profile (percent soil moisture), and surface and subsurface soil moisture anomalies from 2015 to 2022. The dataset is derived by taking predictions from the modified Palmer two-layer model, which are then corrected through the integration of satellite-derived SMAP soil moisture observations (Bolten, Sazib, & Mladenova, 2021). The assimilation of the SMAP imagery is done using an Ensemble Kalman Filter (EnKF) method. The filter is designed to correct model-based predictions for damaging impacts due to rainfall-forcing errors. This applies especially to parts of the world without exhaustive rain gauge instrumentation (Bolten, Sazib, & Mladenova, 2018c). The correction is of great importance as the quality of the assimilation depends on the accuracy of observational and model estimates, meaning that proper evaluation of the soil moisture uncertainty is vital for the best integration of the satellite observations (Maggioni, Anagnostou, & Reichle, 2012)")),
                                         
                                         #br(),
                                         
                                         column(
                                           h1(strong("PICES Data")),
                                           align="justify",
                                           width = 6,
                                           withMathJax(),
                                           title = h1(strong("PICES Data")),
                                           p("The data come from two nationally representative household surveys, called the PICES, conducted by ZIMSTAT: first, from June 2011 to May 2012, and second, from January to December 2017. The PICES surveys are well suited to construct multidimensional poverty indices because they include information at the household and individual levels and are collected repeatedly. The surveys were conducted in the eight provinces of Zimbabwe and in the cities of Harare and Bulawayo. The number of usable observations (households) is 29,748 in 2011–2012 (23,843 rural and 5,905 urban) and 31,193 in 2017 (25,525 rural and 5668 urban). Survey weights and household size are employed to obtain national, provincial, and rural-urban representation. Both survey instruments are virtually identical across the two waves. They include household demographics, education, employment, healthcare, migration, housing characteristics, asset ownership, access to services, and agricultural activities."),
                                           p("The multidimensional poverty index based on the Alkire-Foster method is constructed using eight poverty dimensions consisting of 14 variables relevant to identifying poverty status. Each variable has a specific weight associated with it depending on its contribution to overall poverty and how it pertains to rural and urban communities differently. The first dimension, education, consists of two variables – No Primary Education (Max Educ) and Education Dropout. The No Primary Education (Max Educ) variable refers to nobody in the household having completed primary school. The Education Dropout variable indicates whether the household has a child aged 7-11 who is not enrolled in school. The education dimension receives the greatest weight in the MPI (2 out of 9.5; a weight of 1 for each sub-dimension). The two health variables are also heavily weighted, with the second health dimension being the highest (2 out of 9.5; a weight of 1 for each sub-dimension). These two variables are Chronic Illness, referring to the presence of a chronically ill individual within the household, and Lack of Health Visit, which refers to a household member who has been sick in the past 30 days without receiving necessary healthcare."),
                                           p("Unemployment, defined as a member of the household being unemployed in the last 12 months, is given a weight of 1 for urban households and 0 for rural households since unemployment is less common and is more difficult to identify in rural areas. For housing conditions, two variables are considered: lack of access to electricity and no toilet (in rural areas) or no flush toilet (for urban areas with more developed sanitation). Weights of 0.5 are given to rural residents who lack electricity and a toilet. In urban areas, where lack of electricity indicates a more significant state of deprivation, a weight of one is attributed to electricity. In contrast, the lack of a toilet retains a weight of 0.5."),
                                           p("Two variables reflect living conditions: Poor Water Source and Poor Cooking Fuel, with a weight of 0.5 for each. Rural households are considered deprived if their primary water source is an unprotected well, a river, another unprotected source, or if the water source is 1 km away or farther. In urban areas with more developed water infrastructure, deprivation is defined as not having access to piped water or communal water on-premises (which affects only a small number of households). In rural and urban areas, households are deprived if they use wood or' other' (not electricity, paraffin, gas, coal) as cooking fuel. Lack of Household Assets is given a dimension weight of 1. The stock of household assets is measured by a physical asset index (PAI) and an asset deprivation (D) threshold as follows:
"),
                                           p(" \\(PAI = 2 * motor vehicle + motorcycle + bicycle + television + radio + fridge + landline phone\\)   ", align = "center"),
                                           p("\\( D = 1 \\)", " if ","\\(PAI < 2 \\)", align = "center"),
                                           p("For rural households, agricultural assets are essential indicators of wellbeing and agricultural activity capabilities. The dimension weight is 1.5, with three component variables usually given a weight of 0.5 each. The first variable, Lack of Land, uses a threshold of 0.25 hectares. The second variable on livestock is measured in Tropical Livestock Units (TLU), an indicator of wealth that can be used to insulate households against negative idiosyncratic and covariate shocks. A TLU deprivation threshold of 1 indicates a Lack of Livestock. The third variable is the Lack of Rural Equipment. An agricultural equipment index (AEI) is created as follows:"),
                                           p("\\( AEI = plough + wheelbarrow + scotchcart + tractor + griding mill \\)", align = "center"),
                                           p("\\( D = 1 \\)", " if ","\\(AEI < 1 \\)", align = "center"),
                                           p("The agricultural asset dimension is not included for households in urban areas. The final dimension of wellbeing – with a weight of 1 – is Lack of Access to Services, where remoteness indicates deprivation. Households are considered deprived if they are far from two or more of seven recorded services in the data. The distance thresholds employed are 5 km for a primary school, 15 km for a secondary school, 15 km for a hospital, 5 km for shops, 6 km for a hammer mill, 15 km for a post office, and 5 km for a bus stop, respectively. These distance thresholds are halved in urban areas, where services tend to be closer, but distance still represents a barrier to access. **Note: The livestock data were unavailable in the 2011-12 wave, so a weight of zero has been assigned to this variable. More details can be found here:", a(href="https://dspgtools.shinyapps.io/dspg21zimbabwe/","Using PICES Data to Visualize District Level Multidimensional Poverty in Zimbabwe",target='_blank'), ".")
                                           
                                           ))),
                            
                            tabPanel(strong("Methodology"),
                                     
                                      fluidRow(
                                        column(
                                          12,
                                          #align="justify",
                                          h1(strong("Overview Of Methodology")),
                                          img(src = "Method.png", style = "display: inline; border: 0px solid #C0C0C0; margin-left: auto; margin-right: auto;", width = "85%"), align ="center",
                                          div(tags$caption("Figure 5: Overview of Methodology"),align="center")
                                        ),
                                        br(),
                                       column(
                                         h3(strong("Remote Sensed Data Methods")),
                                         align="justify",
                                         withMathJax(),
                                         title = h3(strong("Remote Sensed Data Methods")),
                                         width = 6,
                                         em(h4("Processing Remote Sensed Data")),
                                         h4(strong("Enhanced Vegetation Index")),
                                         p("The Enhanced Vegetation Index (EVI) has a dynamic range and high sensitivity in dense vegetation. It is particularly suitable for mapping the cropping density in Zimbabwe. As high EVI values correspond to crops at their peak growth stage, we used maximum EVI values in each agro-ecological region during the growing seasons in 2010-11 and 2016-17 to observe the variation in crop growth in different periods. We aggregated the daily EVI to the monthly level. We then conduct a spatial mapping of the EVI to agro-ecological regions in Zimbabwe. In our analysis, we focus on the growing seasons and patterns that are ideal for maize. Past literature defines the growing season of maize to start in October and continue into May the following year (Nyakudya & Stroosnijder, 2011). We will use this growing season to analyze this and all other climatic indicators."),
                                         br(),
                                         h4(strong("Precipitation")),
                                         p("Our geospatial analysis of precipitation occurs by agro-ecological region. We aggregate the 3-hour precipitation data to the daily and monthly levels. We analyze these data with respect to the ideal conditions and growing season for maize. Similar to previous analyses of precipitation, we see that the Northern region typically receives the most rainfall. In contrast, the Southern region receives less (Nkomozepi & Chung, 2012). A season must see 600-700 mm of rain for successful maize production(Mugyio et al., 2021). If a season receives a maximum of 1000 mm of precipitation, yields may increase. However, yields may decline if the total rainfall exceeds 1000 mm for a season (Mushore et al., 2016). In regions with rainfed agriculture, the timing of the most optimal planting period can dictate the success of a season's yield. Planting prematurely can lead to crop failure, particularly if there are extensive dry spells in the middle of the growing season. Alternatively, planting late can reduce the growing season. Both scenarios will result in reduced yield (Mugyio et al., 2021)."),
                                         p("In addition to the total amount of rainfall, we also examine the length of dry spells, another condition that may affect maize production. We construct measures of dry and wet days. A day is dry if the precipitation value is less than 2.95mm. A wet day is a day receiving more than 2.95mm of rain (Mugyio et al., 2021). We then use these wet and dry days to construct dry spells. Dry spell analysis is important in assessing the performance of the growing seasons. A dry spell consists of 14 or more consecutive dry days (Mupangwa et al., 2011; Mugiyo et al., 2021)."),
                                         
                                         
                                         br(),
                                         h4(strong("Soil Moisture")),
                                                h5(strong("Why Soil Moisture?")),
                                                p("Appropriate Surface soil moisture levels are necessary for the success of planting and harvesting activities. For most crops, too little soil moisture during planting stifles seed germination. Too much soil moisture prevents fieldwork as heavy machinery cannot access the field (Bolten et al.,  2018). We have chosen to focus on surface soil moisture during the first 30 days of the growing season since this is when most planting activities occur.")
                                         ,
                                         
                                                p("The germination of maize seeds is dependent to a large extent on soil and environmental conditions. Warm, moist conditions result in seedling emergence at 6 to 10 days, while cool or dry conditions slow emergence to two weeks or longer. The optimum moisture level of the soil is approximately 60% of the total capacity, while optimum soil texture is between 10-30% clay content. Maize grows best in fertile, deep, well-drained soils where total annual rainfall is greater than 500mm. Maize is susceptible to drought and waterlogging; therefore, poorly drained soils should be avoided. Furthermore, drought during silking and tasseling, which occurs during the four weeks spanning flowering, can lead to high yield losses; therefore, some form of water conservation is beneficial."),
                                         
                                         p("Surface soil moisture is the water in the upper 10cm of soil that responds quickly to heavy precipitation and rapidly drying events (Drought.gov, 2022). For our dataset, the surface soil moisture is assumed to hold a maximum of one inch of water, meaning the top layer soil depth varies based on soil texture. Appropriate Surface soil moisture levels are necessary for the success of planting and harvesting activities for most crops, with too little soil moisture during planting stifling the seed germination and too much soil moisture preventing fieldwork or heavy machinery access to the field (Bolten et al., 2018). To be specific, soil moisture levels of:"),
                                        #br(), 
p("-   20-25mm are best for germinating and emergence of a new crop but can halt fieldwork or damage a newly seeded crop in the wet environment for a prolonged period."),

p("-   15-20mm are best for vigorous field activity."),

p("-   10mm or less will not support the early growth potential for a newly emerged crop or seed germination (Bolten et al., 2018)."),
                                         tags$br(),
                                       ),
                                       column(
                                         h3(strong("Multidimensional Poverty Index (MPI) Methodology")),
                                         align="justify",
                                         withMathJax(),
                                         title = h3(strong("Multidimensional Poverty Index (MPI) Methodology")),
                                         width = 6,
                                         em(h4("A Brief Overview Of The Mathematics Behind The Multidimensional Poverty Index")), tags$br(),
                                         p("The methodology for determining the multidimensional poverty 
       indices proposed by Alkire and Foster in 2011 involve a matrix with \\(n\\) 
       rows and \\(d\\) columns, where \\(n\\) is the number of people within the 
       state and \\(d\\) is the number of dimensions for assessing poverty. There 
       are three main measurements denoted on the \\(M\\) scale: \\(M_{0}, M_{1}\\) and \\(M_{2}\\).
       The A-F method employed in this study utilizes the eight dimensions of poverty introduced in the Data tab. A given individual is considered poor if their total number of deprivations is greater than a prescribed threshold, \\(k\\). The multiple dimensions of poverty can be decomposed to their original measures of the individual variables."),
                                         tags$br(),
                                         p("The \\(M_{0}\\) index is known as the Adjusted Headcount Ratio. The simple headcount
       ratio is simply the number of individuals considered to be poor divided by
       the entire population. The \\(M_{0}\\) index adjusts for the multidimensionality
       of the algorithm by multiplying the simple headcount ratio, \\(H\\), by the 
       average deprivation share, \\(A\\). This metric can be thought of as a more
       accurate measure of the simple headcount ratio."),
                                         tags$br(),
                                         p("The \\(M_{1}\\) index is known as the Adjusted Poverty Gap. This examines the distance
       between the prescribed threshold, \\(k\\), and an individual","'","s true number of 
       deprivations. This helps examine the subset of poor individuals to efficiently
       assess which individuals are the poorest in the country."),
                                         tags$br(),
                                         p("The \\(M_{2}\\) index is known as the Adjusted Poverty Severity. This is
       simply the square of the distance between a poor individual and the poverty
       threshold, \\(k\\). The advantage of using this metric is that it weights
       poorer individuals who fall farther from the poverty line more heavily to 
       provide a more obvious descriptor for the poorest people in a given area."),
                                         tags$br(),
                                       #),
                                       box(
                                         width = 6,
                                         h5(strong("Headcount Ratio")),
                                         h3("\\(H = \\frac{n_{poor}}{n_{pop}}\\)"),
                                         tags$br(),
                                         h5(strong("Average Deprivation Share")),
                                         h3("\\(A = \\frac{n_{deprivations}}{n_{potential}}\\)"),
                                         tags$br(),
                                         h5(strong("Deprivation Threshold")),
                                         h5(em("\\(k\\) = Threshold (If an index is above the threshold, k, then the individual is considered poor)")),
                                         tags$br(),
                                         h5(strong("Dimensional Aggregation")),
                                         h4("\\(D_{total} = \\sum_{i=1}^{d}\\sum_{j=1}^{v_{d}} w_{i, j}\\)"),
                                         em(p("\\(d = \\) Number of Dimensions")),
                                         em(p("\\(v_{d} = \\) Number of variables for a Specific Dimension")),
                                         em(p("\\(w_{i,j} = \\) Weight of a Specific Variable for a Specific Dimension"))
                                         
                                         
                                       ),
                                       box(
                                         width = 6,
                                         h5(strong("Poverty Index")),
                                         h4("\\(M_{0}= H * A\\)"),
                                         tags$br(),
                                         h5(strong("Adjusted Poverty Gap")),
                                         h4("\\(M_{1} = μ(g^{1}(k))\\)"),
                                         h4("\\(g^{1}_{i} = k - \\frac{\\sum deprivations}{\\sum possible\\ deprivations}\\)   if   \\(g^{1}_{i} > 0\\)"),
                                         h4("Else \\(g^{1}_{i} = 0\\)"),
                                         tags$br(),
                                         h5(strong("Adjusted Poverty Severity")),
                                         h4("\\(M_{2} = μ(g^{2}(k))\\)"),
                                         h4("\\(g^{2}_{i} = [k - \\frac{\\sum deprivations}{\\sum possible\\ deprivations}]^{2}\\) if \\(g^{2}_{i} > 0\\)"),
                                         h4("Else \\(g^{2}_{i} = 0\\)")
                                         
                                       ))
                                         )),
                            
                            tabPanel(strong("Resources"), 
                                     fluidPage(
                                       column(6,
                                              align="justify",
                                              h3(strong("Google Earth Engine")),
                                              img(src = "data-google-earth.png", style = "display: inline; float: left;", width = "140px"),
                                              withMathJax(),  
                                              p("Google Earth Engine combines a multi-petabyte catalog of satellite imagery and geospatial datasets with planetary-scale analysis capabilities and makes it available for scientists, researchers, and developers to detect changes, map trends, and quantify differences on the Earth's surface. We used it to collect data on NDVI, EVI, precipitation and Soil moisture in Zimbabwe.")),
                                       
                                       column(6,
                                              align="justify",
                                              h3(strong("Google Maps")),
                                              img(src = "data-gmaps.png", style = "display: inline; float: left;", width = "140px"),
                                              withMathJax(), 
                                              p("Google Maps is a comprehensive web mapping service created by Google. Its goal is to provide an interactive map of all the geographical contents of the world. This resource has a variety of uses, ranging from examining all service locations within a city to finding the quickest route between locations. It provides data at latitude and longitude level. We used Google Maps to visualize weather information behind the Google Earth Engine."))),
                                     column(6,
                                            align="justify",
                                            h3(strong("Google Colab")),
                                            img(src = "colab.png", style = "display: inline; float: left;", width = "140px"),
                                            withMathJax(), 
                                            p("Google Colab Colaboratory, or “Colab” for short, is a product from Google Research. Colab allows anybody to write and execute arbitrary python code through the browser, and is especially well suited to machine learning, data analysis and education. We used Google Colab to gather weather information behind the Google Earth Engine.")),
                                     column(6,
                                            align="justify",
                                            h3(strong("GitHub")),
                                            img(src = "github.png", style = "display: inline; float: left;", width = "140px"),
                                            withMathJax(), 
                                            p("GitHub, Inc. is a provider of Internet hosting for software development and version control using Git. It offers the distributed version control and source code management functionality of Git, plus its own features. We used GitHub to host our data and codes. ")),
                                       
                                     column(6,
                                            align="justify",
                                              h3(strong("2021 DSPG - Zimbabwe Project")),
                                              img(src = "DSPG_black-01.png", style = "display: inline; float: left;", width = "140px"),
                                              withMathJax(), 
                                              p("We used the the MPI computed from the 2021 DSPG Zimbabwe project", a(href="https://dspgtools.shinyapps.io/dspg21zimbabwe/","Using PICES Data to Visualize District Level Multidimensional Poverty in Zimbabwe",target='_blank'), "to analyse the link with weather information from the Google Earth Engine.")),
                                       
                                       column(6,
                                              align="justify",
                                              h3(strong("ZimStat")),
                                              img(src = "zimstat_logo.png", style = "display: inline; float: left;", width = "140px"),
                                              withMathJax(), 
                                              p("Zimbabwe National Statistics Agency (ZimStat) is a corporate body established through the Census and Statistics Act of 2007 and the main source of official statistics in Zimbabwe We used the the national Poverty, Income, Consumption, Expenditure Survey (PICES) conducted in 2011 and 2017."))
                                       
                                     ))
                          ),
                 
                 

                 ## Tab 1---------------------
                 navbarMenu(strong("Remote Sensed Data"), 
                            tabPanel(strong("Enhanced Vegetation Index"),
                                     
                                     fluidRow(h1(strong("Enhanced Vegetation Index"), style ="font-size: 25px;", align = "center"),
                                              style = "margin-left: 10px; margin-right: 10px;",
                                              h3(strong("Maximum Enhanced Vegetation Index During Growing Seasons")),
                                              column(withSpinner(leafletOutput("evi_map_leaflet", height=520)),
                                              align="justify",
                                             
                                         title = "Maximum Enhanced Vegetation Index During Growing Seasons",
                                         width = 8,
                                         height = 600
                                       ),
                                         column(
                                           h3("Description"),
                                           align="justify",
                                           width = 4,
                                           withMathJax(),
                                           title = "Description",
                                           p("This graphic shows a detailed visualization of the Enhanced Vegetation Index for the Zimbabwean agro-ecological regions. The maps show the maximum EVI (which is a measure of the density of crops) during the growing seasons in 2011 and 2017, respectively. Both graphs show that the maximum EVI value is at its highest in Region IIA, which, according to United Nations' Food and Agriculture Organisation, is suitable for intensive farming."), 
                                           p("Region V is next with an EVI that aligns with its production being mostly ranching cattle. Region IV has the lowest maximum EVI value, and the FAO describes it as a farming region suitable for resistant fodder crops (FAO, 2020)."),
                                           p("Compared to the growing season in 2011, the maps show that almost every region has a higher maximum EVI during the growing season of 2017. By solely looking at the data, we can also observe that the EVI during the growing season in 2017 is higher than in 2011, with the approximate minimum value being 0.4 (higher than 0.38 in 2011) and the approximate maximum value being 0.5 (higher than 0.48 from 2011)."))),
                                     
                                     br(),
                                     
                                     fluidRow(
                                       style = "margin-left: 10px; margin-right: 10px;",
                                       h3(strong("Maximum Enhanced Vegetation Index During Growing Seasons")), 
                                          style = "font-size:35px;"
                                       #align = "center",
                                     ),
                                    
                                     fluidRow(
                                       
                                       column(
                                       8,
                                       #withSpinner(slickROutput("my_slick_evi"))
                                       img(src = "Max EVI 2011.png", style = "display: inline; border: 0px solid #C0C0C0; margin-left: auto; margin-right: auto;", width = "75%"), align ="center",
                                       div(tags$caption("Figure: 2011"),align="center")
                                     ),
                                     
                                     column(
                                       align="justify",
                                       width = 4,
                                       withMathJax(),
                                       title = strong("Maximum Enhanced Vegetation Index During Growing Seasons", align="center"),
                                       p(h3("Description")),
                                       p("The line graphs show the variation in maximum EVI in each agro-ecological region during the growing season in the years 2011 and 2017, respectively. We could see a general pattern of descending maximum EVI going from Region I to Region V, which matches up with the initial purpose of zoning. The maximum EVI is at its trough in October, and peaks from January to February."), 
                                       p("The months align well with cropping cycle in Zimbabwe: farmers plow the field in October before sowing; the rainy season comes in November, with higher precipitation, crops grow gradually, and finally are at their peak growth stage during February and March, before the rainy season ends. Compare to the growing season in 2011, the highest maximum EVI value in 2017 is higher for almost all districts. This indicates significantly denser vegetation, thus a higher crop yield.")
                                       
                                     )
                                     
                                     
                                     ),
                                     
                                     fluidRow(
                                       column(
                                         8,
                                         img(src = "Max EVI 2017.png", style = "display: inline; border: 0px solid #C0C0C0; margin-left: auto; margin-right: auto;", width = "75%"), align ="center",
                                         div(tags$caption("Figure: 2017"),align="center")
                                       )
                                     
                                     
                                     )
                                     
                                     
                                       
                                       
                                       ),
                            
                            tabPanel(
                              strong("Precipitation"),
                              fluidRow(
                                style = "margin-left: 100px; margin-right: 100px;",
                                h1(strong("Precipitation Index (TRMM 3B42)"), 
                                   style = "font-size:35px;"),
                                align = "center"
                              ),
                              fluidRow(
                                h1(strong("Monthly Comparison Of Average Rainfall In The Growing Seasons For 2010-11 And 2016-17"), 
                                   style ="font-size: 25px;"),
                                style = "margin-left: 100px; margin-right: 100px;",
                                
                              ),
                              fluidRow(
                                style = "margin-left: 0px; margin-right: 0px;",
                                column(12, slickROutput("my_slick"), offset = 0, br(),br(),br())
                                
                              ),
                              fluidRow(
                                align="justify",
                                style = "margin-left: 100px; margin-right: 100px;",
                                column(12, p(h3("Description")) , 
                                p("The maps show the spatial distribution of the monthly average daily rainfall. Each profile compares the same month in 2010-11 with its counterpart in 2016-17. This is done for the first three months of the growing season, which can be seen by advancing through the slide deck using the arros on the left or right. The Northern regions in Zimbabwe are typically the ones to receive the most rainfall. A month-on-month comparison of the 2010-11 and 2016-17 shows more precipitation in the 2016-17 growing period. This could possibly result in a higher yield for maize.")
                              )),
                              fluidRow(
                                style = "margin-left: 100px; margin-right: 100px;",
                                h1(strong("Comparison Of Total Rainfall In The Growing Seasons For 2010-11 And 2016-17"), 
                                   style ="font-size: 25px;"),
                                align = "left"
                              ),
                              fluidRow(
                                style = "margin-left: 5px; margin-right: 100px;",
                                img(src = "totalrainfall_compared.png", height = "100%", width = "100%", style = "display: inline; border: 0px solid #C0C0C0; margin-left: auto; margin-right: auto;", width = "100%"), align ="center",
                                align = "left"
                              ),
                              fluidRow(
                                align="justify",
                                style = "margin-left: 100px; margin-right: 100px;",
                                column(10, p(h3("Description")) , 
                                       p("The maps show the total rainfall during the two growing seasons. Comparing the two growing seasons shows that every agro-ecological region received more rainfall in 2016-17. This is consistent with the research literature, which indicates that Zimbabwe experienced severe drought during 2015-2016. Rainfall patterns are also consistent with other research, showing that the North-East regions typically receive more rainfall than their South-Western counterparts (Nkomozepi & Chung, 2012). The ideal range of rainfall for maize production is 600-700 mm, with an excess of 1000 mm potentially leading to a decline in maize yields. This means that in both growing seasons, All regions except for Region V met the minimum amount of rain necessary for an average maize yield, with Region I having an excess of rain in growing season 2016-17 that may lead to decreases in yield.  
                              "))
                              ),
                              
                              
                              fluidRow(
                                style = "margin-left: 100px; margin-right: 100px;",
                                h1(strong("Comparison of Average Rainfall between Growing Seasons"), 
                                   style ="font-size: 25px;"),
                                align = "left"
                              ),
                              
                             
                              fluidRow(
                                style = "margin-left: 70px; margin-right: 100px;",
                                column(
                                  12,
                                  img(src = "av-line-compared.png", height = "100%", width = "100%", style = "display: inline; border: 0px solid #C0C0C0; margin-left: auto; margin-right: auto;", width = "100%"), align ="center",
                                  
                                ),
                                align = "center",
                                
                                br(),
                                
                                
                              ),
                        
                              fluidRow(
                                align="justify",
                                style = "margin-left: 100px; margin-right: 100px;",
                                column(12, p(h3("Description")) , 
                                       p("These graphs show the total rainfall during the two growing seasons by agro-ecological regions. They depict the same patterning explained above with the maps. A higher amount of rainfall in 2016-17. Regions I, II, and III received the most rainfall."))
                                ),
                              
                            
                              fluidRow(
                                style = "margin-left: 100px; margin-right: 100px;",
                                h1(strong("Comparison of Dryspells between Growing Seasons"),
                                   style = "font-size: 25px;"),
                                align = "left"
                              ),
                              fluidRow(
                                style = "margin-left: 70px; margin-right: 100px;",
                                column(12, img(src = "dry_compared.png", height = "100%", width = "100%")),
                                align = "center"
                              ),
                              
                              fluidRow(
                                style = "margin-left: 100px; margin-right: 100px;",
                                column(12, 
                                       align="justify",
                                       p(h3("Description")) ,
                                       p("The bar plot shows the number of dry spells across regions during the growing season for maize; A dry spell is described to be a consecutive series of dry days between 10 to 20 days or 20 days or more. There were more dry spells in the 2010-11 growing season."))
                              ),
                              
                              br(),
                              
                              fluidRow(
                                h3(strong("Correlations with District-Level Rainfall"), 
                                   style = "font-size:35px;"), align="center",
                                style = "margin-left: 0px; margin-right: 0px;",
                                #h3(strong("District Level Rainfall"), align = "left"),
                                column(
                                  align="justify",
                                  width = 12,
                                  withMathJax(),
                                  title = "Description",
                                  p(""))
                                
                              ),
                              
                              fluidRow(
                                column(6,
                                       #style = "margin-left: 5px; margin-right: 100px;",
                                       img(src = "TotalRainfallByDistrict2011.png", style = "display: inline; border: 0px solid #C0C0C0; margin-left: auto; margin-right: auto;", width = "100%"), align ="center",
                                       div(tags$caption(""),align="center")
                                       #align = "left"
                                ),
                                column(6,
                                       #style = "margin-left: 5px; margin-right: 100px;",
                                       img(src = "TotalRainfallByDistrict2017.png", style = "display: inline; border: 0px solid #C0C0C0; margin-left: auto; margin-right: auto;", width = "100%"), align ="center",
                                       div(tags$caption(""),align="center")
                                       #align = "left"
                                       
                                )),
                              
                              
                              
                              fluidRow(
                                align="justify",
                                style = "margin-left: 100px; margin-right: 100px;",
                                column(12, p(strong("Description")) , 
                                       p("
               We further disaggregate rainfall to the district level for our statistical analysis. The district level maps show the total rainfall during the two growing seasons.  
               Every district received a higher amount of rainfall in 2016-17 than in 2010-11. 
               This is consistent  with literature, which indicates that Zimbabwe experienced a severe drought during 2015-2016. 
               Rainfall patterns are also consistent, as North-East districts (regions) typically receive more rainfall than their South-Western counterparts (Nkomozepi & Chung, 2012). 
               For maize production, the ideal range of rainfall is 600-700 mm with excess of 1000 mm potentially leading  to a decline in maize yields. 
               This means that in both growing seasons, All regions except for districts falling within Region V met the minimum amount of rain necessary for an average maize yield, 
               with districts falling within Region I having an excess of rain in growing season 2016-17 that may lead to decreases in yield.   
               "))
                                
                              ),
                              
                              
                              
                              ),
                            
                              tabPanel(strong("Soil Moisture"),
                                     tabsetPanel(
                                     tabPanel("Surface Soil Moisture",
                                              fluidRow(style = "margin: 6px;",
                                                       #h1(strong("Surface Soil Moisture "), align = "center"),
                                                       p("", style = "padding-top:10px;")
                                                   
                                                       
                                              ),
                                              
                                              
                                     fluidRow(
                                       box(withSpinner(leafletOutput("SurfMapGraph", height=520)),
                                           title = h1(strong("Average Surface Soil Moisture During Planting for the 2016-17 Growing Season"), 
                                                      style ="font-size: 25px;"),
                                           width = 8,
                                           height = 600
                                       ),
                                       box(
                                         align="justify",
                                         width = 4,
                                         withMathJax(),
                                         title = "Description",
                                         p("This map shows the average surface soil moisture (in mm) by agro-ecological region. The average is taken over the first 30 days of the 2016-17 growing season, which takes place from November 19th to December 19th of 2016. Regions I, IIa, IIb, and III have dry surface soil moisture (10-15mm), while regions IV and V have extremely dry surface soil moisture (>10mm)."),
                                         p("These soil moisture levels suggest that while farmers in all regions of Zimbabwe are likely to experience stifled germination upon planting during the 2016/2017 growing season, farmers in regions IV and V are likely to be more impacted than their counterparts in the other regions."))),
                                     
                                     
                                     fluidRow(
                                       box(img(src = "SurfaceSoilBar.png", height = "100%", width = "100%"),
                                           title = h1(strong("Surface Soil Moisture Period Conditions"), 
                                                      style ="font-size: 25px;"),
                                          width = 8,
                                       ),
                                       box(
                                         align="justify",
                                         withMathJax(),
                                           width = 4,
                                           title = "Description",
                                           p("The bar chart shows the number of 3-day periods by region that fall within each of the four surface soil moisture condition categories. The number of 3-day periods is taken over the first 30 days of the 2016-17 growing season, which takes place from November 19th to December 19th of 2016."),
                                           p("The chart shows that none of the regions experienced any wet periods, and Region V is unique in not experiencing any ideal periods. Furthermore, Regions I through III all had either four or five ideal 3-day periods, while Region IV only had two. This aligns with the previous visualization's findings of Regions I through III having more soil moisture on average than regions IV and V."))),
                                     fluidRow(
                                       box(img(src = "SurfaceSoilLine.png", height = "100%", width = "100%"),
                                           title = h1(strong("Surface Soil Moisture Across Time"),
                                                      style ="font-size: 25px;"),
                                           width = 8,
                                       ),
                                       box(
                                         align="justify",
                                         withMathJax(),
                                           width = 4,
                                           title = "Description",
                                           p("This line chart shows that the ranking of soil moisture levels by region remains largely consistent over the time period. The difference between the region with the highest soil moisture and the region with the lowest roughly doubles over the first 30 days of the growing season."),
                                           p("In addition, while regions I , II, and III experience soil moisture levels above the extremely dry threshold (10mm) as early as November 24th, regions IV and V do not reach those levels until December 9th.")))
                                     
                                     
                                     ),
                                     

#                                       #fluidRow(
#                                         box(withSpinner(plotOutput("SurfBarGraph")),
#                                             title = "Soil Moisture At Planting",
#                                             width = 8
#                                             #height = 600
#                                         ),
#                                         box(
#                                           width = 4,
#                                           withMathJax(),
#                                           title = "Description",
#                                           p("This histogram chart shows the number of 3-day periods by region that fall within each of the four soil condition categories. The number of 3-day periods is  # taken over the first 30 days of the 2016-17 growing season, which takes place from November 19th to December 19th of 2016."),
#                                           p("From this visualization, we can see that none of the regions experienced any wet periods, and Region V is unique in not experiencing any ideal periods. #  Furthermore, Regions I through III all had either four or five ideal 3-day periods, while Region IV only had two. This aligns with the previous visualization’s findings of Regions I through III having more #  soil moisture on average than regions IV and V.")),
#                                         
#                                       #fluidRow(  
#                                       box(withSpinner(plotOutput("SurfLineGraph")),
#                                             title = "Soil Moisture at Planting Times",
#                                             width = 8
#                                             #height = 600
#                                         ),
#                                         box(
#                                           width = 4,
#                                           withMathJax(),
#                                           title = "Description",
#                                           p("This line chart shows by region the surface soil moisture in mm over the first 30 days of the 2016-17 growing season, which takes place from November 19th to #  December 19th of 2016. From this visualization we can see that the ranking of soil moisture levels by region remains largely consistent over the period."),
#                                           p("the difference between the region with the highest soil moisture and the region with the lowest roughly doubles over the first 30 days of the growing season. In#   addition, while regions I – III experience soil moisture levels above the extremely dry threshold (10mm) as early as November 24th*, regions IV and V do not reach those levels until December 9th*."))),
                                        

                  tabPanel("Percent Soil Moisture",
                           fluidRow(
                             #h1(strong("Percent Soil Moisture"), align = "center"),
                                    box(withSpinner(leafletOutput("PercMapGraph", height=520)),
                                        title = h1(strong("Average Percent Soil Moisture Map"),
                                                   style ="font-size: 25px;"),
                                        width = 8,
                                        height = 600
                                    ),
                                    box(align="justify",
                                      withMathJax(),
                                        width = 4,
                                        title = "Description",
                                        p("This visualization shows the average Percent soil moisture by Zimbabwe's natural regions. The average is taken over the 2016-17 growing season after the first 30 days, which takes place from December 19th of 2016 to May 29th of 2017. From the visual, we can see that all regions except for V have ideal percent soil moisture (50-80%) and the average percent soil moisture decreases in order when going from region I to V. "))),
                           fluidRow(
                             box(img(src = "PercentSoilBar.png", height = "100%", width = "100%"),
                                 title = h1(strong("Percent Soil Moisture period conditions"),
                                            style ="font-size: 25px;"),
                                 width = 8,
                             ),
                             box(align="justify",
                               withMathJax(),
                                 width = 4,
                                 title = "Description",
                                 p("This Grouped Bar chart shows the number of 3-day periods by region that fall within each of the four percent soil moisture condition categories. The number of 3-day periods is taken over the 2016-17 growing season after the first 30 days, which takes place from December 19th of 2016 to May 29th of 2017."),
                                 p("From the chart, we can see that the number of wet days decreased in order when going from Region I to V, with region V having none at all. Furthermore, we can see that surprisingly, Region V has the largest number of Ideal days, but this may be offset by it also having the largest number of Extremely dry days as well as having no wet days."))),
                           fluidRow(
                             box(img(src = "PercentSoilLine.png", height = "100%", width = "100%"),
                                 title = h1(strong("Percent Soil Moisture Across Time"),
                                            style ="font-size: 25px;"),
                                 width = 8,
                             ),
                             box(align="justify",
                               withMathJax(),
                                 width = 4,
                                 title = "Description",
                                 p("This line chart shows by region the percent soil moisture over the 2016-17 growing season after the first 30 days, which takes place from December 19th of 2016 to May 29th of 2017."),
                                 p("From the chart, we can see that the relative rankings for regions percent soil moisture remains consistent for the most part although there is a greater range in values across regions at the start of the period than at the end. Furthermore, while all regions see a sharp decrease in percent soil moisture around the biggining of March, region V’s decrease begins around 2 weeks before the other regions. "))))
                           

                                        
                                      ))
                            ), 
                                      
                            
                            
                            #),

                 ## Tab 4 ------
                 navbarMenu(strong("Poverty & Components"), 
                 tabPanel(strong("Multidimensional Poverty Index"),
                    
                          tabPanel(title = "2011",
                                   fluidRow(h1(strong("District-Level Multidimensional Poverty Index"), align = "center")),
                                   fluidRow(
                                     column(
                                       align="justify",
                                       #h3("Description"),
                                       width = 12,
                                       withMathJax(),
                                       title = "Description",
                                       p("This graphic shows a map of poverty (MPI) by ",strong("district")," for 2011 and 2017. There are three layers to this graph:
                                      \\(M_{0}\\), \\(M_{1}\\), and \\(M_{2}\\)."), 
                                       tags$ul(  
                                         tags$li("\\(M_{0}\\) is the ",strong("adjusted headcount ratio")," designed by",a(href="https://ophi.org.uk/research/multidimensional-poverty/alkire-foster-method/","Sabina Alkire and James Foster",target="_blank"),
                                                 " and considers all of the dimensions described in the methodology section."),
                                         tags$li("\\(M_{1}\\)
                                      is the ",strong("adjusted poverty gap")," an index to show how far below the poor people are from the poverty line."),
                                         tags$li("\\(M_{2}\\) is the ",strong("square of the adjusted poverty gap."),"By squaring the poverty gaps, this measure puts a higher weight on those who are farther away from the poverty line. Thus, this index measures severity of poverty.")
                                         
                                       ),
                                       p("In this study, we use MPI that has been calculated using \\(k=3\\) as the threshold.
For more details, please refer to ", a(href="https://dspgtools.shinyapps.io/dspg21zimbabwe/","Using PICES Data to Visualize District Level Multidimensional Poverty in Zimbabwe",target='_blank'), ".")),
                                     
                                     
                                   ),
                                   
                                   
                                   fluidRow(#h1(strong("Multidimensional Poverty Index"), align = "center"),
                                            column(
                                              h3("Multidimensional Poverty Index 2011"),
                                              withSpinner(leafletOutput("MPI_map_2011", height=520)),
                                                title = "Multidimensional Poverty Index 2011",
                                                width = 6,
                                                height = 600
                                            ),
                                            column(
                                              h3("Multidimensional Poverty Index 2017"),
                                              withSpinner(leafletOutput("MPI_map_2017", height=520)),
                                                title = "Multidimensional Poverty Index 2017",
                                                width = 6,
                                                height = 600
                                                
                                            )),
                                   
                                   fluidRow(
                                   column(
                                     align="justify",
                                     h3("Descriptive Analysis"),
                                     withMathJax(),
                                     title = strong("Descriptive Analysis"),
                                     width = 12,
                                     p("\\(M_{0}\\): When we focus on the \\(M_{0}\\) index, we can see that for our k-threshold value, a large portion of the population can be considered multidimensionally poor. The greater Harare and Bulawayo areas have low \\(M_{0}\\) values for our k-threshold of 3."),
                                     
                                     p("\\(M_{1}\\): When we focus on the depth of poverty (\\(M_{1}\\) index ), for our k-threshold value, poverty throughout much of Zimbabwe can be considered deep."),
                                     p("\\(M_{2}\\): A look at the \\(M_{2}\\) values of the index reveals much of the same. Our k-threshold value render high rates of poverty severity across a large proportion of Zimbabwe’s population."),
                                     p("")
                                   ))
                                   
                                   
                                   )),
                 
                 tabPanel(strong("Components of the MPI"),
                          
                          tabPanel(title = "\\(M_{0}\\)",
                                   
                                   fluidRow(h1(strong("Components of the MPI"), align = "center"),
                                            column(
                                              align="justify",
                                            width = 12,
                                            withMathJax(),
                                            title = "Description",
                                            p("This graphic shows a map of the relevant components of MPI at the", strong("district-level"),". Our study uses ", strong("district-level")," measures of various MPI components to explore their association with the three remotely sensed indices of concern. We limit only to those components that assign an equal weight to urban and rural households. Otherwise, components with unequal weights may over-/underestimate the severity of deprivation if a district contains predominantly urban (rural) households. For example, component Lack of Land is assigned a weight of zero to urban households, so districts (such as Bulawayo) that are mostly urban will appear to be less deprived in this component than more rural districts. The components we examine in this study are:"),
                                              tags$ul(
                                                tags$li(strong("No Primary Education (Max Education):")," nobody in the household having completed primary school"),
                                                tags$li(strong("Education Dropout (Unenrolled Child):")," an indicator variable for whether the household has a child aged 7-11 who is not enrolled in school."),
                                                tags$li(strong("Chronic Illness:")," the presence of a chronically ill individual within the household"),
                                                tags$li(strong("Lack of Health Visit:"), "a household member who has been sick in the past 30 days without receiving a necessary healthcare"),
                                                tags$li(strong("Lack of Household Assets:"), "stock of household assets is measured by a physical asset index (PAI) and an asset deprivation (D) threshold "),
                                                tags$li(strong("Lack of Access to Services:"), "lack of access to electricity and no toilet (in rural areas) or no flush toilet (for urban areas with more developed sanitation).")
                                                
                                              ),
                                             # p("Max Education, Education Dropout, Chronic Illness, Lack of Health Visit, Lack of Household Assets and Lack of Access to Services."),
                                              p("Note: for our district-level analysis, a grey-filled area with an NA means that no districts fulfill the criteria chosen. These results are presented for the incidence  (\\(M_{0}\\))."),#, gap (\\(M_{1}\\)), and severity of poverty (\\(M_{2}\\))."),
                                            p("In this study, we use MPI that has been calculated using \\(k=3\\) as the threshold.
For more details on the gap (\\(M_{1}\\)), and severity of poverty (\\(M_{2}\\)), please refer to ", a(href="https://dspgtools.shinyapps.io/dspg21zimbabwe/","Using PICES Data to Visualize District Level Multidimensional Poverty in Zimbabwe",target='_blank'), ".")
                                                                                )),
                                   
                                   fluidRow(
                                     box(withSpinner(leafletOutput("compo_MPI_11", height = 520)),
                                       title = "Components of the MPI for 2011",
                                       width = 6,
                                       height = 600
                                     ),
                                     box(withSpinner(leafletOutput("compo_MPI_17", height = 520)),
                                         title = "Components of the MPI for 2017",
                                         width = 6,
                                         height = 600
                                     )))),
                 
                 
                 
                 ),

                ## Tab 5 ------
              navbarMenu(strong("Analysis"), 
              tabPanel(strong("Correlations"),
                       fluidRow(
                         h1(strong("Correlations"), 
                            style = "font-size:35px;"), align="center",
                         style = "margin-left: 0px; margin-right: 0px;",
                         column(12
                                
                                
                                )),
                       
                       fluidRow(
                         column(
                           
                           align="justify",
                           width = 12,
                           withMathJax(),
                           title = strong("Summary Statistics and Correlations", align="center"),
                           p(h3("District-Level Correlation Matrices")),
                           p("We examined the correlation between our climate indices and our measures of poverty and its socioeconomic components at the ", strong("district-level"),". The following matrices present the Pearson Correlation Coefficients of the weather indices and these poverty measures. The statistical significance can be gleaned by observing the color coding within the matrix. Overall, total rainfall in the growing season exhibits consistently signficant correlation with the poverty measures. For example, in 2011, the correlation coefficient between total rainfall and poverty was -0.295 which means that as rainfall increases the incidence of poverty decreases. The coefficient is also statistically significant at the 5 percent level."))
                       ),
                       
                       fluidRow(
                         column(
                           6,
                           div(tags$caption("Table 2: Correlation Matrix for PICES 2011"),align="left"),
                           img(src = "MPICorr2011.png", height = "100%", width = "100%", style = "display: inline; border: 0px solid #C0C0C0; margin-left: auto; margin-right: auto;", width = "100%"), align ="center"
                        
                         ),
                         column(
                           6,
                           div(tags$caption("Table 3: Correlation Matrix for PICES 2017"),align="left"),
                           img(src = "MPICorr2017.png", height = "100%", width = "100%", style = "display: inline; border: 0px solid #C0C0C0; margin-left: auto; margin-right: auto;", width = "90%"), align ="center"
                           
                           
                           ),
                         
                         br(),
                         br(),
                         
                       ),
                       

                       
                        
                       
                    ),
              
              tabPanel(strong("Regressions"),
                       
                       tabsetPanel(
                         tabPanel(strong("Selected Regressions"),
                       
                       fluidRow(
                         h1(strong("Regressions"), 
                            style = "font-size:35px;"), align="center",
                         style = "margin-left: 0px; margin-right: 0px;",
                         column(12
                                
                                
                         )),
                       
                       fluidRow(
                         h4(strong("District-Level MPI & Precipitation"),align="left", 
                            style = "margin-left: 100px; margin-right: 100px;"),
                         column(
                           10,
                           style = "margin-left: 100px; margin-right: 100px;",
                           align="justify",
                           withMathJax(),
                           p("In this section, we present the results from regression analysis of total precipitation (measured in 100 mm) on MPI and its selected components, using", strong("district-level"), "data. We estimate the following regression model using Ordinary Least Squares (OLS) Estimation method:"),
                           p("\\(poverty_{i}\\ = \\beta_{0}\\ + year_{i} \\beta_{1}\\ + rain_{i} \\beta_{2}\\ + \\epsilon\\) where \\(i\\) denotes the districts and ϵ is the error term."),
                           p("\\(poverty_{i}\\) denotes the dependent variables: Poverty Headcount Ratio (\\(M_{0}\\)), Poverty Gap (\\(M_{1}\\)), Square of Poverty Gap (\\(M_{2}\\)) and the MPI components - No Primary Education (Max Educ), Chronic Illness, Lack of Household Assets and Lack of Access to Services."),
                           p("\\(year_{i}\\) is a dummy variable that takes the value 0 if the year is 2011 and 1 if the year is 2017."),
                           p("\\(rain_{i}\\) represents monthly cumulative precipitation (in 100 mm) from the start of planting in November to the end of the growing season in May.")
                           
                         )),
                       br(),
                       br(),
                       fluidRow(
                         h4(strong("Selected Regressions"),align="left", 
                            style = "margin-left: 100px; margin-right: 100px;"),
                         style = "margin-left: 0px; margin-right: 0px;",
                         column(8, slickROutput("my_slick7")),
                         column(4,
                                align="justify",
                                p(
                                  "
                               Figure 1 presents the estimated coefficients of monthly cumulative precipitation (in 100 mm) for poverty headcount ratio (\\(M_{0}\\)). All else constant, an additional 100 mm of rain in the
                               first month of the growing season (Nov-Dec) is associated with a a decrease in poverty headcount by -0.029 units. This estimated coefficient is statistically significant at the 10 percent
                               level. Similarly, cumulative rainfall across all the months of the growing season has a negative association with poverty headcount, ceteris paribus, and the estimated coefficients
                               are always statistically significant. The greatest absolute magnitude of the coefficient occurs during the first month of planting, suggesting that sufficient early rainfall may have
                               important implications for the socio-economic conditions of the people.
                               "
                                ),
                       
                                p(
                                  "
                               Figure 2 presents the estimated coefficients of monthly cumulative precipitation (in 100 mm) for adjusted poverty gap (\\(M_{1}\\)). Similar to Figure 1, the coefficients are all negative and
                               statistically significant, meaning that more rainfall is associated with a lower adjusted poverty gap. Again, we see that the estimated coefficient of rainfall in the first month of planting
                               (Nov-Dec) has the highest absolute magnitude.
                               "
                                ),
                                p(
                                  "
                              Figure 3: presents the estimated coefficients of monthly cumulative precipitation (in 100 mm) for adjusted poverty severity or the square of adjusted poverty gap (\\(M_{2}\\)). Once again, the coefficients are all negative and
                              statistically significant, meaning that more rainfall is associated with a lower adjusted poverty severity.
                               "
                                ),
                                p(
                                  "
                               Figures 4 – 7 presents the estimated coefficients of monthly cumulative precipitation (in 100 mm) for the selected MPI components. Rainfall has an important association with these measures of
                               deprivation as all coefficients are consistently negative and statistically significant. The associations between rainfall in the first month of planting (Nov-Dec) and Max Educ. and Lack of Access to Services
                               are particularly prominent relative to that with cumulative monthly rainfall in later months of the season.
                               "
                                )
                       
                       
                       ))),
                       
                       tabPanel(strong("Sensitivity Analysis 1"),
                                
                                fluidRow(
                                  h1(strong("Analysis on Rural Areas Only"), 
                                     style = "font-size:35px;"), align="center",
                                  style = "margin-left: 0px; margin-right: 0px;",
                                  column(8, slickROutput("my_slick8")),
                                  column(4,
                                         align="justify",
                                         p("Sensitivity Analysis: We conducted additional sensitivity analysis to check the robustness of our findings pertaining to the association between poverty measures and precipitation."),
                                         p("Since literacy rate in Zimbabwe is quite high at 88.69 percent, and primary education completion rate is 89 percent, adjusted MPI measures were generated by replacing the “No Primary Education” component with a “No High School Education” component defined as no one in the household having completed high school."),
                                         p("The regression coefficients using the adjusted MPI measures are presented in Figures 1B, 2B and 3B. When compared to Figures 1, 2 and 3, respectively, we observe that the overall trend is consistent. However, in some cases the associations between cumulative monthly rainfall and the adjusted MPI measures are slightly less significant."))
                                  )
                                
                         
                       ),
                       
                       tabPanel(strong("Sensitivity Analysis 2"),
                                
                                fluidRow(
                                  h1(strong("Using the Adjusted MPI Measure"), 
                                     style = "font-size:35px;"), align="center",
                                  style = "margin-left: 0px; margin-right: 0px;",
                                  column(8, slickROutput("my_slick9")),
                                  column(4,
                                         align="justify",
                                         p("We also estimated the adjusted rural MPI measures that were calculated using only the sub-sample of rural households from PICES. The coefficient estimates from the regression models are presented in Figures 1A, 2A and 3A."),
                                         p("Once again, our findings remain consistent - more rainfall is associated with lower rural poverty headcount, poverty gap, as well as squared poverty gap.")
                                         )
                                ))
                       
                       
                       )
              
 
              )),



#                 ## Tab 2------
#                 navbarMenu(strong("Multidimensional Poverty Index (MPI)"), 
#                            tabPanel(strong("Multidimensional Poverty Index"),
#                                     
#                                     
#                                     tabPanel(title = "2011",
#                                     fluidRow(h1(strong("Multidimensional Poverty Index"), align = "center"),
#                                       box(withSpinner(leafletOutput("MPI_map_2011", height=520)),
#                                         title = "Multidimensional Poverty Index 2011",
#                                         width = 6,
#                                         height = 600
#                                       ),
#                                       box(withSpinner(leafletOutput("MPI_map_2017", height=520)),
#                                           title = "Multidimensional Poverty Index 2017",
#                                           width = 6,
#                                           height = 600
#                                         
#                                       )
#                                         ),
#                                     
#                                     fluidRow(
#                                       column(
#                                         align="justify",
#                                         h3("Description"),
#                                         width = 6,
#                                         withMathJax(),
#                                         title = "Description",
#                                         p("This graphic shows a detailed visualization of MPI across 60 administrative districts. Zimbabwe currently has 59 administrative districts. However, at the time #the 2011 PICES was being conducted, there were 60 administrative districts in Zimbabwe. For this reason, we conduct district-level analysis in our study using the 60 districts. There are three layers to #this graph:
#                                      \\(M_{0}\\), \\(M_{1}\\), and \\(M_{2}\\)."), 
#                                         tags$ul(  
#                                           tags$li("\\(M_{0}\\) is the ",strong("adjusted headcount ratio")," designed by",a(href="https://ophi.org.uk/research/multidimensional-poverty/alkire-foster-method#/","Sabina Alkire and James Foster",target="_blank"),
#                                                   " and considers all of the dimensions described in the methodology section."),
#                                           tags$li("\\(M_{1}\\)
#                                      is the ",strong("adjusted poverty gap")," an index to show how far below the poor people are from the poverty line."),
#                                           tags$li("\\(M_{2}\\) is the ",strong("square of the adjusted poverty gap."),"By squaring the poverty gaps, this measure puts a higher weight on those who are #farther away from the poverty line. Thus, this index measures severity of poverty.")
#                                           
#                                         ),
#                                         p("In this study, we use MPI that has been calculated using k=3 as the threshold.
#For more details, please refer to ", a(href="https://dspgtools.shinyapps.io/dspg21zimbabwe/","Using PICES Data to Visualize District Level Multidimensional Poverty in Zimbabwe",target='_blank'), ".")),
#                                       
#                                       column(
#                                         align="justify",
#                                         h3("Descriptive Analysis"),
#                                         withMathJax(),
#                                         title = strong("Descriptive Analysis"),
#                                         width = 6,
#                                         p("\\(M_{0}\\)"),
#                                         p("Looking at the poverty index and focusing on the \\(M_{0}\\) index, we can see that for our k-threshold value, a large portion of the population can be #considered multidimensionally poor. The greater Harare and Bulawayo areas have low \\(M_{0}\\) values for low k-thresholds. Still, their \\(M_{0}\\) values for higher k-thresholds are above the national #average, implying that while those districts are better on average, some of the most poverty-stricken households reside within their bounds (particularly the Epworth district)."),
#                                         
#                                         p("\\(M_{1}\\)"),
#                                         p("When we focus on the depth of poverty (\\(M_{1}\\) index ), for our k-threshold value, poverty throughout much of Zimbabwe can be considered deep."),
#                                         p("\\(M_{2}\\)"),
#                                         p("A look at the \\(M_{2}\\) values of the original index reveals much of the same. Our k-threshold value render high rates of poverty severity across a large #proportion of Zimbabwe’s population."),
#                                         p("")
#                                       )
#                                     ))),
#                            
#                            
#                            
#                            tabPanel(strong("Components of the MPI"),
#                                     
#                                     tabsetPanel(
#                                       tabPanel(title = "\\(M_{0}\\)",
#                                     fluidRow(h1(strong("Components of the MPI"), align = "center"),
#                                       box(withSpinner(leafletOutput("compo_MPI_11", height = 520)),
#                                         title = "Components of the MPI for 2011",
#                                         width = 6,
#                                         height = 600
#                                       ),
#                                       box(withSpinner(leafletOutput("compo_MPI_17", height = 520)),
#                                           title = "Components of the MPI for 2017",
#                                           width = 6,
#                                           height = 600
#                                       )),
#                                         
#                                     fluidRow(
#                                     box(
#                                           
#                                           width = 12,
#                                           withMathJax(),
#                                           title = "Description",
#                                           p("This graphic shows a detailed visualization of the relevant components of MPI at the district-level. 
#Our study uses district-level measures of various MPI components to explore their association with the three remotely sensed indices of concern. We limit only to those components that assign an equal #weight to urban and rural households. Otherwise, components with unequal weights may over-/underestimate the severity of deprivation if a district contains predominantly urban (rural) households. For #example, component Lack of Land is assigned a weight of zero to urban households, so districts (such as Bulawayo) that are mostly urban will appear to be less deprived in this component than more rural #districts. The components we examine in this study are: Max Education, Education Dropout, Chronic Illness, Lack of Health Visit, Lack of Household Assets and Lack of Access to Services."),
#                                           p("Note: for our district-level analysis, a grey-filled area with an NA means that no districts fulfill the criteria chosen. These results are presented for the #incidence ( (\\(M_{0}\\)), gap (\\(M_{1}\\)), and severity of poverty (\\(M_{2}\\)).")
#                                           ))),
#                                     
#                                     tabPanel(title = "\\(M_{1}\\)",
#                                              fluidRow(h1(strong("Components of the MPI"), align = "center"),
#                                                       box(withSpinner(leafletOutput("compo_MPI_11_m1", height = 520)),
#                                                           title = "Components of the MPI for 2011",
#                                                           width = 6,
#                                                           height = 600
#                                                       ),
#                                                       box(withSpinner(leafletOutput("compo_MPI_17_m1", height = 520)),
#                                                           title = "Components of the MPI for 2017",
#                                                           width = 6,
#                                                           height = 600
#                                                       )),
#                                              
#                                              fluidRow(
#                                                box(
#                                                  align="justify",
#                                                  width = 12,
#                                                  withMathJax(),
#                                                  title = "Description",
#                                                  p("This graphic shows a detailed visualization of the relevant components of MPI at the district-level. 
#Our study uses district-level measures of various MPI components to explore their association with the three remotely sensed indices of concern. We limit only to those components that assign an equal #weight to urban and rural households. Otherwise, components with unequal weights may over-/underestimate the severity of deprivation if a district contains predominantly urban (rural) households. For #example, component Lack of Land is assigned a weight of zero to urban households, so districts (such as Bulawayo) that are mostly urban will appear to be less deprived in this component than more rural #districts. The components we examine in this study are: Max Education, Education Dropout, Chronic Illness, Lack of Health Visit, Lack of Household Assets and Lack of Access to Services."),
#                                                  p("Note: for our district-level analysis, a grey-filled area with an NA means that no districts fulfill the criteria chosen. These results are presented #for the incidence ( (\\(M_{0}\\)), gap (\\(M_{1}\\)), and severity of poverty (\\(M_{2}\\)).")
#                                                ))),
#                                     
#                                     tabPanel(title = "\\(M_{2}\\)",
#                                              fluidRow(h1(strong("Components of the MPI"), align = "center"),
#                                                       box(withSpinner(leafletOutput("compo_MPI_11_m2", height = 520)),
#                                                           title = "Components of the MPI for 2011",
#                                                           width = 6,
#                                                           height = 600
#                                                       ),
#                                                       box(withSpinner(leafletOutput("compo_MPI_17_m2", height = 520)),
#                                                           title = "Components of the MPI for 2017",
#                                                           width = 6,
#                                                           height = 600
#                                                       )),
#                                              
#                                              fluidRow(
#                                                box(
#                                                  align="justify",
#                                                  width = 12,
#                                                  withMathJax(),
#                                                  title = "Description",
#                                                  p("This graphic shows a detailed visualization of the relevant components of MPI at the district-level. 
#Our study uses district-level measures of various MPI components to explore their association with the three remotely sensed indices of concern. We limit only to those components that assign an equal #weight to urban and rural households. Otherwise, components with unequal weights may over-/underestimate the severity of deprivation if a district contains predominantly urban (rural) households. For #example, component Lack of Land is assigned a weight of zero to urban households, so districts (such as Bulawayo) that are mostly urban will appear to be less deprived in this component than more rural #districts. The components we examine in this study are: Max Education, Education Dropout, Chronic Illness, Lack of Health Visit, Lack of Household Assets and Lack of Access to Services."),
#                                                  p("Note: for our district-level analysis, a grey-filled area with an NA means that no districts fulfill the criteria chosen. These results are presented #for the incidence (\\(M_{0}\\)), gap (\\(M_{1}\\)), and severity of poverty (\\(M_{2}\\)).")
#                                                )))
#                                     
#                                     
#                                     
#                                     ))),
#                            
#                            
                 
                 ## Tab 3------------------
#navbarMenu(strong("MPI and Indices"),
#           tabPanel(strong("Summary Statistics"),
#                    fluidRow(
#                      h1(strong("Summary Statistics"), 
#                         style = "font-size:35px;"), align="center",
#                    style = "margin-left: 0px; margin-right: 0px;",
#                    column(12, slickROutput("my_slick5"))),
#                    
#                    
#                    fluidRow(
#                      column(
#                        7,
#                        img(src = "stats_v2_2011.png", style = "display: inline; border: 0px solid #C0C0C0; margin-left: auto; margin-right: auto;", width = "75%"), align ="center",
#                        div(tags$caption("Figure: Correlation Matrix for PICES 2011"),align="center")
#                      ),
#                      
#                      column(
#                        align="justify",
#                        width = 5,
#                        withMathJax(),
#                        title = strong("Summary Statistics and Correlations", align="center"),
#                        p(h3("Summary Statistics")),
#                        p("The table presents summary statistics for the variables of interest in our statistical analysis using 60 district-level data. The average poverty headcount (M0) was higher in #2017 (0.325) compared to that in 2011 (0.272). Similarly, average adjusted poverty gap (M1) and average adjusted poverty severity (M2) were also higher in 2017 compared to those in 2011. A cursory #screening of the selected MPI components indicates that the main drivers of this discrepancy were Chronic Illness, Lack of HH Assets and Lack of Services.  
#
#In contrast, the growing season of 2016-17 received more rain and encountered fewer dry spells on average compared to the growing season of 2010-11. As a result of good seasonal rain, the average maximum #EVI is also higher in 2016-17 compared that in 2010-11."), 
#                        
#                      )
#                      
#                    ),
#                    
#                    fluidRow(
#                      column(
#                        7,
#                        img(src = "stats_2017.png", style = "display: inline; border: 0px solid #C0C0C0; margin-left: auto; margin-right: auto;", width = "75%"), align ="center",
#                        div(tags$caption("Figure: Correlation Matrix for PICES 2017"),align="center")
#                      ),
#                      
#                      column(
#                        align="justify",
#                        width = 5,
#                        withMathJax(),
#                        title = strong("Summary Statistics and Correlations", align="center"),
#                        p(h3("Correlation Matrices")),
#                        
#                        p("The following matrices present the Pearson Correlation Coefficients of the weather indices and the MPI measures and components. Overall, total rainfall in the growing season #exhibit the strongest correlation with the poverty measures relative to the other weather indices. For example, in 2011, the correlation coefficient of total rainfall and M0 was -0.295 which means that #rainfall and poverty headcount are inversely associated. The coefficient is also statistically significant at the 5 percent level.")
#                        
#                      )
#                      
#                    ),
#                    
#                    #fluidRow(
#                    #column(12,
#                    #p("Summary Statistics and Correlations"))),
#                    
#                    
#                    
#                    
#                    
#                    
#                    
#           ),
#                    
#                    
#                    
#           tabPanel(strong("MPI & Precipitation"),
#                    tabsetPanel(
#                      tabPanel(strong("Tables"),
#                    fluidRow(
#                      h1(strong("MPI & Precipitation"),align="center", 
#                         style = "font-size:35px;"),
#                      column(
#                        10,
#                        style = "margin-left: 100px; margin-right: 100px;",
#                        align="justify",
#                        withMathJax(),
#                      p("In this section, we present the results from regression analysis of total precipitation (measured in 100 mm) on MPI and its selected components, using district-level data. We #estimate the following regression model using Ordinary Least Squares (OLS) Estimation method:"),
#                      p("\\(poverty_{i}\\ = \\beta_{0}\\ + year_{i} \\beta_{1}\\ + rain_{i} \\beta_{2}\\ + \\epsilon\\) where \\(i\\) denotes the districts and ϵ is the error term."),
#                      p("\\(poverty_{i}\\) denotes the dependent variables: Poverty Headcount Ratio (\\(M_{0}\\)), Poverty Gap (\\(M_{1}\\)), Square of Poverty Gap (\\(M_{2}\\)) and the MPI components - #Max Educ, Chronic Illness, Lack of Household Assets and Lack of Access to Services."),
#                      p("\\(year_{i}\\) is a dummy variable that takes the value 0 if the year is 2011 and 1 if the year is 2017."),
#                      p("\\(rain_{i}\\) represents monthly cumulative precipitation (in 100 mm) from the start of planting in November to the end of the growing season in May.")
#                      
#                      )),
#                    
#                    br(),
#                    
#                    fluidRow(
#                      style = "margin-left: 0px; margin-right: 0px;",
#                      column(8, slickROutput("my_slick2")),
#                      column(4,
#                             align="justify",
#                             p(
#                               "
#                               Table 2:
#                        This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In #2017 PICES
#                        , the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this #graph:
#                               "
#                             ),
#                             p(
#                               "Table 2:
#                        This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In #2017 PICES
#                        , the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this #graph:"
#                             ),
#                             p(
#                               "Table 3:
#                        This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In #2017 PICES
#                        , the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this #graph:"
#                             ),
#                             p(
#                               "Table 4:
#                        This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In #2017 PICES
#                        , the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this #graph:"
#                             )
#                      )
#                      
#                    )
#                  )
#                  ,
#                  tabPanel(strong("Regressions"),
#                    fluidRow(
#                      style = "margin-left: 0px; margin-right: 0px;",
#                      column(8, slickROutput("my_slick7")),
#                      column(4, 
#                             p(
#                               "
#                               Figure 1: 
#                               This presents the estimated coefficients of monthly cumulative precipitation (in 100 mm) for poverty headcount ratio (\\(M_{0}\\)). All else constant, an additional 100 mm of #rain in the 
#                               first month of the growing season (Nov-Dec) corresponds to a decrease in poverty headcount by -0.029 units. This estimated coefficient is statistically significant at the 10 #percent 
#                               level. Similarly, cumulative rainfall across all the months of the growing season has a negative association with poverty headcount, ceteris paribus, and the estimated #coefficients 
#                               are always statistically significant. The greatest absolute magnitude of the coefficient occurs during the first month of planting, suggesting that sufficient early rainfall #may have 
#                               important implications for the socio-economic conditions of the people.  
#                               "
#                             ),
#                             p(
#                               "
#                               Figure 2: 
#                               This presents the estimated coefficients of monthly cumulative precipitation (in 100 mm) for adjusted poverty gap (\\(M_{1}\\)). Similar to Figure 1, the coefficients are all #negative and 
#                               statistically significant, meaning that more rainfall corresponds to a lower adjusted poverty gap. Again, we see that the estimated coefficient of rainfall in the first month #of planting 
#                               (Nov-Dec) has the highest absolute magnitude.  
#                               "
#                             ),
#                             p(
#                               "
#                              Figure 3:  
#                              This presents the estimated coefficients of monthly cumulative precipitation (in 100 mm) for adjusted poverty severity or the square of adjusted poverty gap (\\(M_{2}\\)). #Once again, the coefficients are all negative and 
#                              statistically significant, meaning that more rainfall corresponds to lower adjusted poverty severity.  
#                               "
#                             ),
#                             p(
#                               "
#                               Figures 4 – 7: 
#                               This presents the estimated coefficients of monthly cumulative precipitation (in 100 mm) for the selected MPI components. Rainfall clearly has an important association with #these measures of 
#                               deprivation as all coefficients are consistently negative and statistically significant. The associations between rainfall in the first month of planting (Nov-Dec) and Max #Educ. and Lack of Access to Services 
#                               are particularly prominent relative to that with cumulative monthly rainfall in later months of the season. 
#                               "
#                             )
#                             )
#                    )
#                    )
#                  )
#           
#                    
#           ),
#           tabPanel(strong("MPI & EVI"),
#                    fluidRow(
#                      h1(strong("MPI & EVI"), 
#                         style = "font-size:35px;"), align="center",
#                      style = "margin-left: 0px; margin-right: 0px;",
#                      column(8, slickROutput("my_slick3")),
#                      column(4,
#                             align="justify",
#                             p(
#                               "Table 1:
#                      This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In #2017 PICES, 
#                      the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph#:"
#                             ),
#                             p(
#                               "Table 2:
#                        This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In #2017 PICES
#                        , the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this #graph:"
#                             )
#                      )
#                      
#                    )
#                    
#           ),
#           tabPanel(strong("MPI & Soil Moisture"),
#                    fluidRow(
#                      h1(strong("MPI & Soil Moisture"), 
#                         style = "font-size:35px;"), align="center",
#                      style = "margin-left: 0px; margin-right: 0px;",
#                      column(8, slickROutput("my_slick4")),
#                      column(4,
#                             align="justify",
#                             p(
#                               "Table 1:
#                      This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In #2017 PICES, 
#                      the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph#:"
#                             ),
#                             p(
#                               "Table 2:
#                        This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In #2017 PICES
#                        , the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this #graph:"
#                             )
#                      )
#                      
#                    )
#                    
#           )
#           
#           
#           
#           
#           
#),
#                            
#                            
#                 
                  ## Tab Takeaways --------------

                 tabPanel(strong("Takeaways"),
                          
                          fluidRow(style = "margin-left: 80px; margin-right: 80px;",
                                   column(3),
                                   column(6,
                                          align="justify",
                                          h1(strong("Takeaways"),align="center"),
                                          p("Last year, the VT DSPG in collaboration with ZimStat and the World Bank, created Poverty Indices at the district level for 2011 and 2017. We used national household PICES data to conduct this exercise. This year, we show how the district level measures can be used to answer questions related to climate change."),
                                          p("We do so by extending the reach of the PICES data using freely available data remotely sensed data from Google Earth Engine. We study precipitation, EVI, and soil moisture and their association with poverty and other district-level socioeconomic variables."),
                                          p(" We find that for:"),
                                          p(strong("Enhanced Vegetation Index (EVI):"), "The maximum EVI is highest in Region IIA, which, according to United Nations’ Food and Agriculture Organization, is suitable for intensive farming. Region IV has the lowest maximum EVI value, and the FAO describes it as the “semi-extensive” farming region, suitable for resistant fodder crops."),
                                          p(strong("Precipitation:"), "Zimbabwe generally follows previous analysis of its weather pattern but as it relates to precipitation the Northern regions are typically the ones to receive the most rainfall. The Southern region on the other hand receive less rainfall."),
                                          p(strong("Soil Moisture:"), "The 2016-17 average soil moisture readings show that regions I through III have dry, and regions IV and V have extremely dry, surface soil moisture levels during planting time. These levels suggest that farmers in all regions are likely to experience stifled germination upon planting. However, farmers in regions IV and V are likely to be more severely impacted."),
                                          p(strong("Correlations With Poverty:"), "We observe a negative correlation between total rainfall and the average  poverty rate in both PICES years, However, we do not find a significant correlation between other weather indices and the average  poverty rate."),
                                          p("In conclusion, the PICES data at lower levels of geographical granularity can be combined with publicly available data to study many interesting and policy-relevant questions. In this project, we demonstrate how district level PICES data can be used to assess the usefulness of remotely sensed data specific to Zimbabwe."),
                                          p("")
                                   )
                                   
                          )
                          
                 ),
                 
                 
                 
                 
                 ## Tab DSPG Team------------------------------------------------
                 tabPanel(strong("Our Team"), 
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   h1(strong("Project Team"), align = "center"),
                                   br(),
                                   h4(strong("VT Data Science for the Public Good"), align = "center"),
                                   align="justify",
                                   p("The", a(href = 'https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html', 'Data Science for the Public Good (DSPG) Young Scholars program', target = "_blank"),
                                     "is a summer immersive program offered by the", a(href = 'https://aaec.vt.edu/index.html', 'Virginia Tech Department of Agricultural and Applied Economics'), 
                                     "In its third year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges 
                                               around critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to 
                                               determine how information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program highlights, 
                                               how to apply, and our annual symposium, please visit", 
                                     a(href = 'https://aaec.vt.edu/content/aaec_vt_edu/en/academics/undergraduate/beyond-classroom/dspg.html#select=1.html', 'the official VT DSPG website.', target = "_blank")),
                                   p("", style = "padding-top:10px;")
                          ),
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   column(4, align = "center",
                                          h4(strong("Graduate Fellows")),
                                          p("", style = "padding-top:10px;"),
                                          img(src = "team-Leo.jpg", style = "display: inline;  border: 0px solid #C0C0C0;", width = "150px"),
                                          #p("", style = "padding-top:10px;"),
                                          #p(a(href = 'https://www.linkedin.com/in/leonard-allen-quaye', 'Leonard-Allen Quaye', target = '_blank'), "(Virginia Tech, Agricultural and Applied Economics, Ph.D.)"),
                                          #br(),
                                          img(src = "team-Poonam.jpg", style = "display: inline; border: 0px solid #C0C0C0;", width = "150px"),
                                          #p("", style = "padding-top:10px;"),
                                          #p(a(href = 'https://www.bse.vt.edu/people/grad-students/poonam-tajanpure.html', 'Poonam Tajanpure', target = '_blank'), "(Virginia Tech, Biological Systems Engineering, Ph.D.)"),
                                          #img(src = "team-Frankie.png", style = "display: inline; border: 0px solid #C0C0C0;", width = "150px"),
                                          #img(src = "team-Ari.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          #img(src = "team-Josue.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          
                                          #p("", style = "padding-top:10px;"),
                                          p(a(href = 'https://www.linkedin.com/in/leonard-allen-quaye', 'Leonard-Allen Quaye', target = '_blank'), "(Virginia Tech, Agricultural and Applied Economics);"),
                                          p(a(href = 'https://www.bse.vt.edu/people/grad-students/poonam-tajanpure.html', 'Poonam Tajanpure', target = '_blank'), "(Virginia Tech, Biological Systems Engineering)."),
                                          #p(a(href = 'https://www.linkedin.com/in/frankie-ruoyu-fan/?lipi=urn%3Ali%3Apage%3Ad_flagship3_people_connections%3BBiz9W9pbRcO00B0bou%2F2vg%3D%3D', 'Frankie Fan', target = '_blank'), "(Smith College & Brown University, Math and Data Science);"),
                                          #p(a(href = 'https://www.linkedin.com/in/ari-l-12b151123/?lipi=urn%3Ali%3Apage%3Ad_flagship3_people_connections%3B5WMwWerMTvefiu%2Fq85Z5mw%3D%3D', 'Ari Liverpool', target = '_blank'), "(Virginia Tech, Applied Economics Management);"),
                                          #p( a(href = 'https://www.linkedin.com/in/josue-navarrete-36a6321b4/?lipi=urn%3Ali%3Apage%3Ad_flagship3_people_connections%3B5WMwWerMTvefiu%2Fq85Z5mw%3D%3D', 'Josue Navarrete', target = '_blank'), "(MiraCosta College, Computer Science Major)."),
                                          
                                          p("", style = "padding-top:10px;")
                                          
                                   ),
                                   column(4, align = "center",
                                          h4(strong("Undergraduate Interns")),
                                          p("", style = "padding-top:10px;"),
                                          #img(src = "team-Leo.png", style = "display: inline;  border: 0px solid #C0C0C0;", width = "150px"),
                                          #img(src = "team-Poonam.png", style = "display: inline; border: 0px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-Frankie.jpg", style = "display: inline; border: 0px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-Ari.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-Josue.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          
                                          p("", style = "padding-top:10px;"),
                                          #p(a(href = 'https://www.linkedin.com/in/leonard-allen-quaye', 'Leonard-Allen Quaye', target = '_blank'), "(Virginia Tech, Agricultural and Applied Economics, Ph.D.);"),
                                          #p(a(href = 'https://www.bse.vt.edu/people/grad-students/poonam-tajanpure.html', 'Poonam Tajanpure', target = '_blank'), "(Virginia Tech, Biological Systems Engineering, Ph.D.);"),
                                          p(a(href = 'https://www.linkedin.com/in/frankie-ruoyu-fan/?lipi=urn%3Ali%3Apage%3Ad_flagship3_people_connections%3BBiz9W9pbRcO00B0bou%2F2vg%3D%3D', 'Frankie Fan', target = '_blank'), "(Smith College & Brown University, Math and Data Science);"),
                                          p(a(href = 'https://www.linkedin.com/in/ari-l-12b151123/?lipi=urn%3Ali%3Apage%3Ad_flagship3_people_connections%3B5WMwWerMTvefiu%2Fq85Z5mw%3D%3D', 'Ari Liverpool', target = '_blank'), "(Virginia Tech, Applied Economics Management);"),
                                          p( a(href = 'https://www.linkedin.com/in/josue-navarrete-36a6321b4/?lipi=urn%3Ali%3Apage%3Ad_flagship3_people_connections%3B5WMwWerMTvefiu%2Fq85Z5mw%3D%3D', 'Josue Navarrete', target = '_blank'), "(MiraCosta College, Computer Science Major)."),
                                          
                                          p("", style = "padding-top:10px;")
                                          
                                   ),
                                   column(4, align = "center",
                                          h4(strong("Faculty & Associate Team Members")),
                                          p("", style = "padding-top:10px;"),
                                          img(src = "faculty-posadas.png", style = "display: inline; border: 0px solid #C0C0C0;", width = "150px"),
                                          img(src = "faculty-chen.png", style = "display: inline;  border: 0px solid #C0C0C0;", width = "150px"),
                                          img(src = "faculty-alwang.jpg", style = "display: inline; border: 0px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-naveen.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p("", style = "padding-top:10px;"),
                                          p(a(href = "https://www.linkedin.com/in/briannaposadas/", 'Dr. Brianna Posadas', target = '_blank'), "(Virginia Tech, School of Plant and Environmental Sciences);"),
                                          p(a(href = "https://aaec.vt.edu/people/faculty/chen-susan.html", 'Dr. Susan Chen', target = '_blank'), "(Virginia Tech, Agricultural and Applied Economics);"),
                                          p(a(href = "https://aaec.vt.edu/people/faculty/alwang-jeffrey.html", 'Dr. Jeffrey Alwang', target = '_blank'), "(Virginia Tech, Agricultural and Applied Economics);"),
                                          p( a(href = 'https://www.linkedin.com/in/naveen-abedin-0ab1089a/?lipi=urn%3Ali%3Apage%3Ad_flagship3_people_connections%3BgdZR16ktRcatg1cpCMufuQ%3D%3D', 'Naveen Abedin', target = '_blank'), "(Virginia Tech, Agricultural and Applied Economics)."),
                                          p("", style = "padding-top:10px;")
                                   )
                          ),
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   h4(strong("Project Stakeholders"), align = "center"),
                                   p("The World Bank"),
                                   #p(a(href="https://www.linkedin.com/in/dhiraj-sharma-aa029024/?originalSubdomain=np","Dhiraj Sharma",target='_blank')," (World Bank); "),
                                   # p("TAWANDA CHINGOZHA (STELLENBOSCH UNIVERSITY) ",a(href="https://www.zimstat.co.zw/","(Zimbabwe National Statistics Agency)",target="_blank")),
                                   
                                   p(em("Disclaimer: "),("This project is an academic exercise conducted by VT-Data Science for the Public Good. The findings, interpretations, and conclusions expressed here do not necessarily reflect the views of the World Bank or the Zimbabwe Statistical Agency."))
                                   
                          ),
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   h4(strong("Acknowledgement"), align = "center"),
                                   p("We would like to thank:"),
                                   p(" ",a(href="https://www.researchgate.net/profile/Tawanda-Chingozha","Tawanda Chingoza",target='_blank')," (Stellenbosch University);"),
                                   p(a(href="https://tw.linkedin.com/in/kuo-hao-lai","Kuo-Hao Lai",target='_blank')," (Virginia Tech, Computer Science, MEng); "),
                                   p(a(href="http://www.uwyo.edu/wygisc/people/yang_di/di-short-cv.html","Dr. Di Yang",target='_blank')," (Wyoming Geographic Information Science Center - WyGISC); "),
                                   p("We also thank Grown Chirongwe of Zimbabwe National Statistical Agency (ZimStat) for providing 2011 and 2017 PICES data for this project.")
                                   
                          )
                 ),
                 
                 
                 
                 ## References-------------------------------
                 tabPanel(strong("References"), value = "references",
                          column(3),
                          column(6, 
                                 h1(strong("References"), align = "center"),
                                 p("Barron, J., Rockström, J., Gichuki, F., & Hatibu, N. (2003). Dry spell analysis and maize yields for two semi-arid locations in east Africa. Agricultural and Forest Meteorology, 117(1-2), 23–37. https://doi.org/10.1016/s0168-1923(03)00037-6 "),
                                 p("Bolten, J. D., Sazib, N., & Mladenova, I. E. (2018). *Surface_Soil_Moisture_SMAP.pdf*. NASA Goddard Space Flight Center Retrieved from https://gimms.gsfc.nasa.gov/SMOS/SMAP/SoilMoisture_Profile_SMAP.pdf"),
                                 p("Milne, G., Mekonnen, A. F., & Benitez Ponce, P. C. (2019). Zimbabwe-Climate Smart Agriculture Investment Plan."),
                                 p("Mugiyo, H., Mhizha, T., Chimonyo, Vimbayi. G. P., & Mabhaudhi, T. (2021). Investigation of the optimum planting dates for maize varieties using a hybrid approach: A case of Hwedza, Zimbabwe. Heliyon, 7(2), e06109. https://doi.org/10.1016/j.heliyon.2021.e06109 "),
                                 p("Mupangwa, W., Walker, S., & Twomlow, S. (2011). Start, end and dry spells of the growing season in semi-arid southern Zimbabwe. Journal of Arid Environments, 75(11), 1097–1104. https://doi.org/10.1016/j.jaridenv.2011.05.011 "),
                                 p("Mushore, T., Manatsa, D., Pedzisai, E., Muzenda-Mudavanhu, C., Mushore, W., & Kudzotsa, I. (2016). Investigating the implications of meteorological indicators of seasonal rainfall performance on maize yield in a rain-fed agricultural system: case study of Mt. Darwin District in Zimbabwe. Theoretical and Applied Climatology, 129(3-4), 1167–1173. https://doi.org/10.1007/s00704-016-1838-2 "),
                                 p("Nkomozepi, T., & Chung, S.-O. (2012). Assessing the trends and uncertainty of maize net irrigation water requirement estimated from climate change projections for Zimbabwe. Agricultural Water Management, 111, 60–67. https://doi.org/10.1016/j.agwat.2012.05.004 "),
                                 p("Nyakudya, I. W., & Stroosnijder, L. (2011). Water management options based on rainfall analysis for rainfed maize (Zea mays L.) production in Rushinga district, Zimbabwe. Agricultural Water Management, 98(10), 1649–1659. https://doi.org/10.1016/j.agwat.2011.06.002 "),
                                 p("Tadross, M. A., Hewitson, B. C., & Usman, M. T. (2005). The Interannual Variability of the Onset of the Maize Growing Season over South Africa and Zimbabwe. Journal of Climate, 18(16), 3356–3372. https://doi.org/10.1175/jcli3423.1 "),
                                 p("TRMM. (2012). Nasa.gov. https://trmm.gsfc.nasa.gov/3b42.html ")

                                
                                 )
                 ),
                 inverse = T)




## Define server logic required---------------------
server <- function(input, output) {
  # Run JavaScript Code
  runjs(jscode)
  

#Reading Tables  for front page
output$table <- renderTable({
    
    table <- read_excel("./data/table.xlsx")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "l", colnames = T, digits = 2)  
  

#correlation for 2011
output$table11 <- renderTable({
  
  table <- read_excel("./data/Correlation matrices.xlsx", 
                      sheet = "Sheet2")
  table
}, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "l", colnames = T, digits = 2) 


#Correlation for 2017
output$table17 <- renderTable({
  
  table <- read_excel("./data/Correlation matrices.xlsx", 
                      sheet = "Sheet3")
  table
}, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "l", colnames = T, digits = 2) 



  
  # EVI OUTPUTS-------
output$evi_map_leaflet <- renderLeaflet({
  leaflet(EVIGrow2011) %>% addTiles() %>%  
    addPolygons(color = ~mypal(MaxEVI), weight = 1, smoothFactor = 0.5, label = paste("Region -", EVIGrow2011$Region,":", round(EVIGrow2011$MaxEVI, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="2011") %>%
    addPolygons(data = EVIGrow2017, color = ~mypal(MaxEVI), weight = 1, smoothFactor = 0.5, label = paste("Region -", EVIGrow2017$Region,":", round(EVIGrow2017$MaxEVI, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="2017") %>% 
    
    addLegend(pal = mypal,position = "bottomright",values = c(EVIGrow2011$MaxEVI, EVIGrow2017$MaxEVI),
              opacity = .6,title= paste("Maximum EVI"))%>%
    
    addPolylines(data = zim_region$geometry, color = "black", opacity = 2, weight = 2,)%>%
    addLayersControl(baseGroups = c("2011", "2017"), 
                     options = layersControlOptions(collapsed = FALSE), position = "topright") %>%
    hideGroup("2017") %>% 
    setView(lat = -19.0154, lng=29.1549 , zoom =6)
})

output$my_slick_evi <- renderSlickR(
  slickR(
    my_images_evi,
    width = "90%"
  )
)


#output$evi_line11 <- renderPlot({
#  GrSs2011Line <- EVI_region_long %>% 
#    filter(Month == "05"|Month == "04"|Month =="03"|Month =="02"|Month =="01"|Month =="10"|Month =="11"|Month =="12", 
#           Year == 2010|Year == 2011) %>% 
#    filter(!(Year == 2010 & Month == "03")) %>% 
#    filter(!(Year == 2010 & Month == "04")) %>%
#    filter(!(Year == 2010 & Month == "05")) %>%
#    filter(!(Year == 2010 & Month == "02")) %>% 
#    filter(!(Year == 2010 & Month == "01")) %>% 
#    filter(!(Year == 2011 & Month == "10")) %>% 
#    filter(!(Year == 2011 & Month == "11")) %>%
#    filter(!(Year == 2011 & Month == "12")) %>% 
#    group_by(Region, Month) %>% 
#    summarise(MaxEVI = max(EVI, na.rm = TRUE)) %>% 
#    mutate(GSOrder = case_when(Month =="10" ~ "1", 
#                               Month =="11" ~ "2",
#                               Month =="12" ~ "3",
#                               Month =="01" ~ "4",
#                               Month =="02" ~ "5",
#                               Month =="03" ~ "6",
#                               Month =="04" ~ "7",
#                               Month =="05" ~ "8"))
#  
#  GrSs2011Line$Month[which(GrSs2011Line$Month=="10")] <- "October"
#  GrSs2011Line$Month[which(GrSs2011Line$Month=="11")] <- "November"
#  GrSs2011Line$Month[which(GrSs2011Line$Month=="12")] <- "December"
#  GrSs2011Line$Month[which(GrSs2011Line$Month=="01")] <- "January"
#  GrSs2011Line$Month[which(GrSs2011Line$Month=="02")] <- "Febuary"
#  GrSs2011Line$Month[which(GrSs2011Line$Month=="03")] <- "March"
#  GrSs2011Line$Month[which(GrSs2011Line$Month=="04")] <- "April"
#  GrSs2011Line$Month[which(GrSs2011Line$Month=="05")] <- "May"
#  
#  GrSs2011Line$Month <- reorder(GrSs2011Line$Month, as.numeric(GrSs2011Line$GSOrder))
#  GrSs2011Line$Month <- as.factor(GrSs2011Line$Month)
#  GrSs2011Line$Region <- as.factor(GrSs2011Line$Region)
#  
# 
#  # Max EVI
#  GrSs2011Line %>% 
#    ggplot(aes(x = Month, y = MaxEVI, group = as.factor(Region), color = as.factor(Region))) +
#    geom_line()+
#    #theme(axis.text.x = element_text(angle = 315)) +
#    scale_colour_discrete(guide = 'none') +
#    scale_x_discrete(expand=c(0, 1)) +
#    geom_dl(aes(label = Region), method = list(dl.combine("first.points", "last.points")), cex = 0.8) +
#    scale_color_viridis_d(option = "H") +
#    labs(title = "Max EVI in Zim During Growing Season 2011", color =  "Region") +
#    xlab("Time(Month)") +
#    ylab("Max EVI")
#  
#})
#
#output$evi_line17 <- renderPlot({
#  GrSs2017Line <- EVI_region_long %>% 
#    filter(Month == "05"|Month == "04"|Month =="03"|Month =="02"|Month =="01"|Month =="10"|Month =="11"|Month =="12", 
#           Year == 2016|Year == 2017) %>% 
#    filter(!(Year == 2016 & Month == "03")) %>% 
#    filter(!(Year == 2016 & Month == "04")) %>%
#    filter(!(Year == 2016 & Month == "05")) %>%
#    filter(!(Year == 2016 & Month == "02")) %>% 
#    filter(!(Year == 2016 & Month == "01")) %>% 
#    filter(!(Year == 2017 & Month == "10")) %>% 
#    filter(!(Year == 2017 & Month == "11")) %>%
#    filter(!(Year == 2017 & Month == "12")) %>% 
#    group_by(Region, Month) %>% 
#    summarise(MaxEVI = max(EVI, na.rm = TRUE)) %>% 
#    mutate(GSOrder = case_when(Month =="10" ~ "1", 
#                               Month =="11" ~ "2",
#                               Month =="12" ~ "3",
#                               Month =="01" ~ "4",
#                               Month =="02" ~ "5",
#                               Month =="03" ~ "6",
#                               Month =="04" ~ "7",
#                               Month =="05" ~ "8"))
#  
#  GrSs2017Line$Month[which(GrSs2017Line$Month=="10")] <- "October"
#  GrSs2017Line$Month[which(GrSs2017Line$Month=="11")] <- "November"
#  GrSs2017Line$Month[which(GrSs2017Line$Month=="12")] <- "December"
#  GrSs2017Line$Month[which(GrSs2017Line$Month=="01")] <- "January"
#  GrSs2017Line$Month[which(GrSs2017Line$Month=="02")] <- "Febuary"
#  GrSs2017Line$Month[which(GrSs2017Line$Month=="03")] <- "March"
#  GrSs2017Line$Month[which(GrSs2017Line$Month=="04")] <- "April"
#  GrSs2017Line$Month[which(GrSs2017Line$Month=="05")] <- "May"
#  
#  GrSs2017Line$Month <- reorder(GrSs2017Line$Month, as.numeric(GrSs2017Line$GSOrder))
#  GrSs2017Line$Month <- as.factor(GrSs2017Line$Month)
#  GrSs2017Line$Region <- as.factor(GrSs2017Line$Region)
#  
#  
#  #write.csv(GrSs2017Line, file = "eviline2017.csv")
#  #GrSs2017Line <- read.csv("./data/agregion/evi/eviline2017.csv")
#  
#  
#  
#  # Max EVI
#  GrSs2017Line %>% 
#    ggplot(aes(x = Month, y = MaxEVI, group = as.factor(Region), color = as.factor(Region))) +
#    geom_line()+
#    #theme(axis.text.x = element_text(angle = 315)) +
#    scale_colour_discrete(guide = 'none') +
#    scale_x_discrete(expand=c(0, 1)) +
#    geom_dl(aes(label = Region), method = list(dl.combine("first.points", "last.points")), cex = 0.8) +
#    scale_color_viridis_d(option = "H") +
#    labs(title = "Max EVI in Zim During Growing Season 2017", color =  "Region") +
#    xlab("Time(Month)") +
#    ylab("Max EVI") 
#  
#  
#})


## PRECIPITATION OUTPUTS
output$my_slick <- renderSlickR(
  slickR(
    my_images,
    width = "90%"
  )
)


## SOIL MOISTURE OUTPUTS-------
output$SurfMapGraph <- renderLeaflet({
  mypal <- colorNumeric(
    palette = "viridis",
    domain = NULL,
    reverse = TRUE)
  
  leaflet(SurfMapDataFin) %>% addTiles() %>%
    addPolygons(color = ~mypal(SurfMapDataFin$AvgSurfaceMoisture), weight = 1, smoothFactor = 0.5, label = paste("Region ", SurfMapDataFin$Region, ":", round(SurfMapDataFin$AvgSurfaceMoisture, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE)) %>%
    addPolylines(data = SurfMapDataFin$geometry, color = "black", opacity = 2, weight = 2) %>% 
    addLegend(pal = mypal,position = "bottomright",values = SurfMapDataFin$AvgSurfaceMoisture, opacity = .6,
              title= paste("Average Surface </br> Soil Moisture (mm)"))
})       
output$SurfBarGraph <- renderPlot({
  ggplot(SurfBarData, aes(fill=time, y=value, x=region)) + 
    geom_bar(position="dodge", stat="identity")+ 
    labs(color="time") +
    xlab("Agro-ecological Region") + ylab("Number Of 3-Day Periods") + 
    ggtitle("Surface Soil Moisture Conditions during Planting in 2016-17 Growing Season") +
    guides(fill=guide_legend(title="Soil Condition")) + labs(caption = "3 Day: NASA-USDA Enhanced SMAP Global") +
    scale_fill_viridis(discrete=TRUE, direction=-1)
  #3day periods within 30 days of 11/19/16 by region and Surf-soil moisture condition
})

output$SurfLineGraph <- renderPlot({
  
  ggplot(SurfLineData, aes(as.Date(newDate), y = value, color = variable)) + 
    geom_line(aes(y = Moisture.1, col = "Region I"), size=1.25) + 
    geom_line(aes(y = Moisture.2, col = "Region IIA"), size=1.25) + 
    geom_line(aes(y = Moisture.3, col = "Region IIB"), size=1.25) + 
    geom_line(aes(y = Moisture.4, col = "Region III"), size=1.25) + 
    geom_line(aes(y = Moisture.5, col = "Region IV"), size=1.25) + 
    geom_line(aes(y = Moisture.6, col = "Region V"), size=1.25) + 
    labs(color="Agro-ecological Region") +
    xlab("Soil Moisture: First 30 Days Of Planting Time") + ylab("Surface Soil Moisture Index (mm)") + 
    ggtitle("Surface Soil Moisture During Planting in 2016-17 Growing Season") +
    theme(plot.title = element_text(hjust = 0.5)) + scale_color_viridis(discrete = TRUE, option = "viridis") +
    scale_x_date(limits = c(Surfmin, Surfmax)) + labs(caption = "3 Day: NASA-USDA Enhanced SMAP Global")  + theme(plot.caption=element_text(hjust = 1))
  
})

output$PercMapGraph <- renderLeaflet({
  mypal <- colorNumeric(
    palette = "viridis",
    domain = NULL,
    reverse = TRUE)
  
  leaflet(PercMapDataFin) %>% addTiles() %>%
    addPolygons(color = ~mypal(PercMapDataFin$AvgPercentMoisture), weight = 1, smoothFactor = 0.5, label = paste("Region ", PercMapDataFin$Region, ":", round(PercMapDataFin$AvgPercentMoisture, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE)) %>%
    addPolylines(data = PercMapDataFin$geometry, color = "black", opacity = 2, weight = 2) %>% 
    addLegend(pal = mypal,position = "bottomright",values = PercMapDataFin$AvgPercentMoisture, opacity = .6,
              title= paste("Average Percent </br> Soil Moisture (mm)"))
})       
output$PercBarGraph <- renderPlot({
  ggplot(PercBarData, aes(fill=time, y=value, x=region)) + 
    geom_bar(position="dodge", stat="identity")+ 
    labs(color="time") +
    xlab("Agro-ecological Region") + ylab("Number Of 3-Day Periods") + 
    ggtitle("Percent Soil Moisture Conditions After Planting in 2016-17 Growing Season") +
    guides(fill=guide_legend(title="Soil Condition")) + labs(caption = "3 Day: NASA-USDA Enhanced SMAP Global") +
    scale_fill_viridis(discrete=TRUE, direction=-1)
  #3day periods within 30 days of 11/19/16 by region and Surf-soil moisture condition
})

output$PercLineGraph <- renderPlot({
  
  ggplot(PercLineData, aes(as.Date(newDate), y = value, color = variable)) + 
    geom_line(aes(y = Moisture.1, col = "Region I"), size=1.25) + 
    geom_line(aes(y = Moisture.2, col = "Region IIA"), size=1.25) + 
    geom_line(aes(y = Moisture.3, col = "Region IIB"), size=1.25) + 
    geom_line(aes(y = Moisture.4, col = "Region III"), size=1.25) + 
    geom_line(aes(y = Moisture.5, col = "Region IV"), size=1.25) + 
    geom_line(aes(y = Moisture.6, col = "Region V"), size=1.25) + 
    labs(color="Agro-ecological Region") +
    xlab("Percent Soil Moisture: After Planting Time") + ylab("Percent Soil Moisture Index") + 
    ggtitle("Percent Soil moisture After Planting in 2016-17 Growing Season") +
    theme(plot.title = element_text(hjust = 0.5)) + scale_color_viridis(discrete = TRUE, option = "viridis") +
    scale_x_date(limits = c(Percmin, Percmax)) + labs(caption = "3 Day: NASA-USDA Enhanced SMAP Global")  + theme(plot.caption=element_text(hjust = 1))
  
})







# output$MapGraph <- renderLeaflet({
#   mypal <- colorNumeric(
#     palette = "viridis",
#     domain = NULL,
#     reverse = TRUE)
#   
#   leaflet(MapDataFin) %>% addTiles() %>%
#     addPolygons(color = ~mypal(MapDataFin$AvgSurfaceMoisture), weight = 1, smoothFactor = 0.5, label = paste("Region ", MapDataFin$Region, ":", round(MapDataFin$AvgSurfaceMoisture# , digits = 3)),
#                 opacity = 1.0, fillOpacity = 0.5,
#                 highlightOptions = highlightOptions(color = "black", weight = 2,
#                                                     bringToFront = TRUE)) %>%
#     addPolylines(data = MapDataFin$geometry, color = "black", opacity = 2, weight = 2) %>% 
#     addLegend(pal = mypal,position = "bottomleft",values = MapDataFin$AvgSurfaceMoisture, opacity = .6,
#               title= paste("Average Soil Moisture (mm)"))
# })       
# output$BarGraph <- renderPlot({
#   ggplot(BarData, aes(fill=time, y=value, x=region)) + 
#     geom_bar(position="dodge", stat="identity")+ 
#     labs(color="time") +
#     xlab("Agro-ecological Region") + ylab("Number Of 3-Day Periods") + 
#     ggtitle("Soil Moisture Conditions In The Planting Time During The 2016-17 Growing Season") +
#     guides(fill=guide_legend(title="Soil Condition")) + labs(caption = "3 Day: NASA-USDA Enhanced SMAP Global") +
#     scale_fill_viridis(discrete=TRUE, direction=-1)
#   #3day periods within 30 days of 11/19/16 by region and Surf-soil moisture condition
# })
# 
# output$LineGraph <- renderPlot({
#   
#   ggplot(LineData, aes(as.Date(newDate), y = value, color = variable)) + 
#     geom_line(aes(y = Moisture.1, col = "Region I"), size=1.25) + 
#     geom_line(aes(y = Moisture.2, col = "Region IIA"), size=1.25) + 
#     geom_line(aes(y = Moisture.3, col = "Region IIB"), size=1.25) + 
#     geom_line(aes(y = Moisture.4, col = "Region III"), size=1.25) + 
#     geom_line(aes(y = Moisture.5, col = "Region IV"), size=1.25) + 
#     geom_line(aes(y = Moisture.6, col = "Region V"), size=1.25) + 
#     labs(color="Agro-ecological Region") +
#     xlab("Soil Moisture: First 30 Days Of Planting Time") + ylab("Surface Soil Moisture Index (mm)") + 
#     ggtitle("Planting Time During The 2016-17 Growing Season") +
#     theme(plot.title = element_text(hjust = 0.5)) + scale_color_viridis(discrete = TRUE, option = "viridis") +
#     scale_x_date(limits = c(min, max)) + labs(caption = "3 Day: NASA-USDA Enhanced SMAP Global")  + theme(plot.caption=element_text(hjust = 1))
#   
# })


#MPI OUTPUTS -----
output$MPI_map_2011 <- renderLeaflet({
  mypal <- colorNumeric(
    palette = "viridis",
    domain = NULL,
    reverse = TRUE)
  
  leaflet(joined_zim) %>% addTiles() %>%  
    addPolygons(color = ~mypal(m0_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name, ":", round(joined_zim$m0_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="M0") %>%
    addPolygons(color = ~mypal(m1_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name, ":", round(joined_zim$m1_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="M1")  %>%  
    addPolygons(color = ~mypal(m2_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name, ":", round(joined_zim$m2_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="M2") %>%
    addPolylines(data = joined_zim$geometry, color = "black", opacity = 2, weight = 2,)%>% 
    setView(lat = -19.0154, lng=29.1549 , zoom =6) %>% 
    addLegend(pal = mypal,position = "bottomright",values = joined_zim$m1_k3,
              opacity = .6,title= paste("2011 MPI")) %>% 
    addLayersControl(baseGroups = c("M0", "M1", "M2"), 
                     options = layersControlOptions(collapsed = FALSE), position = "topright") %>%
    #hideGroup("M0")%>% 
    hideGroup("M1")%>% 
    hideGroup("M2")
})

output$MPI_map_2017 <- renderLeaflet({

  leaflet(joined_zim17) %>% addTiles() %>%  
    addPolygons(color = ~mypal(m0_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name, ":", round(joined_zim17$m0_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="M0") %>%
    addPolygons(color = ~mypal(m1_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name, ":", round(joined_zim17$m1_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="M1")  %>%  
    addPolygons(color = ~mypal(m2_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name, ":", round(joined_zim17$m2_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="M2") %>%
    addPolylines(data = joined_zim$geometry, color = "black", opacity = 2, weight = 2,)%>% 
    setView(lat = -19.0154, lng=29.1549 , zoom =6) %>% 
    addLegend(pal = mypal,position = "bottomright",values = joined_zim$m0_k3,
              opacity = .6,title= paste("2017 MPI")) %>% 
    addLayersControl(baseGroups = c("M0", "M1", "M2"), 
                     options = layersControlOptions(collapsed = FALSE), position = "topright") %>%
    #hideGroup("M0")%>% 
    hideGroup("M1")%>% 
    hideGroup("M2")
})

#M0
output$compo_MPI_11 <- renderLeaflet({
  leaflet(joined_zim) %>% addTiles() %>%  
    addPolygons(fillColor = ~mypal(g0_edu_max_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name, ":", round(joined_zim$g0_edu_max_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="No Primary Education") %>%
    addPolygons(fillColor = ~mypal(joined_zim$g0_edu_dropout_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name, ":", round(joined_zim$g0_edu_dropout_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Education Dropout")  %>%  
    addPolygons(fillColor = ~mypal(joined_zim$g0_hea_chronic_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name, ":", round(joined_zim$g0_hea_chronic_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Chronic Ilness") %>%
    addPolygons(fillColor = ~mypal(joined_zim$g0_hea_visit_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name, ":", round(joined_zim$g0_hea_visit_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Health Visit") %>%
    addPolygons(fillColor = ~mypal(joined_zim$g0_assets_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name, ":", round(joined_zim$g0_assets_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Household Assets") %>%
    addPolygons(fillColor = ~mypal(joined_zim$g0_services_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name, ":", round(joined_zim$g0_services_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Service Access") %>%
    
    addPolylines(data = joined_zim$geometry, color = "black", opacity = 2, weight = 2,)%>% 
    setView(lat = -19.0154, lng=29.1549 , zoom =6) %>% 
    addLegend(pal = mypal,position = "bottomright",values = joined_zim$g0_edu_max_k3,
              opacity = .6,title= paste("MPI Component Value")) %>% 
    addLayersControl(baseGroups = c("No Primary Education","Education Dropout", "Chronic Ilness","Health Visit", "Household Assets","Service Access"), 
                     options = layersControlOptions(collapsed = FALSE), position = "topright") %>%
    #hideGroup("M0")%>% 
    hideGroup("Education Dropout")%>% 
    hideGroup("Chronic Ilness") %>% 
    hideGroup("Health Visit") %>% 
    hideGroup("Household Assets") %>% 
    hideGroup("Service Access")
})

#M1
output$compo_MPI_11_m1 <- renderLeaflet({
  leaflet(joined_zim) %>% addTiles() %>%  
    addPolygons(fillColor = ~mypal(g1_edu_max_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name, ":", round(joined_zim$g1_edu_max_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="No Primary Education") %>%
    addPolygons(fillColor = ~mypal(joined_zim$g1_edu_dropout_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name, ":", round(joined_zim$g1_edu_dropout_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Education Dropout")  %>%  
    addPolygons(fillColor = ~mypal(joined_zim$g1_hea_chronic_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name, ":", round(joined_zim$g1_hea_chronic_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Chronic Ilness") %>%
    addPolygons(fillColor = ~mypal(joined_zim$g1_hea_visit_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name, ":", round(joined_zim$g1_hea_visit_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Health Visit") %>%
    addPolygons(fillColor = ~mypal(joined_zim$g1_assets_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name, ":", round(joined_zim$g1_assets_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Household Assets") %>%
    addPolygons(fillColor = ~mypal(joined_zim$g1_services_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name, ":", round(joined_zim$g1_services_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Service Access") %>%
    
    addPolylines(data = joined_zim$geometry, color = "black", opacity = 2, weight = 2,)%>% 
    setView(lat = -19.0154, lng=29.1549 , zoom =6) %>% 
    addLegend(pal = mypal,position = "bottomright",values = joined_zim$g1_edu_max_k3,
              opacity = .6,title= paste("MPI Component Value")) %>% 
    addLayersControl(baseGroups = c("No Primary Education","Education Dropout", "Chronic Ilness","Health Visit", "Household Assets","Service Access"), 
                     options = layersControlOptions(collapsed = FALSE), position = "topright") %>%
    #hideGroup("M0")%>% 
    hideGroup("Education Dropout")%>% 
    hideGroup("Chronic Ilness") %>% 
    hideGroup("Health Visit") %>% 
    hideGroup("Household Assets") %>% 
    hideGroup("Service Access")
})


#M2
output$compo_MPI_11_m2 <- renderLeaflet({
  leaflet(joined_zim) %>% addTiles() %>%  
    addPolygons(fillColor = ~mypal(g2_edu_max_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name, ":", round(joined_zim$g2_edu_max_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="No Primary Education") %>%
    addPolygons(fillColor = ~mypal(joined_zim$g2_edu_dropout_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name, ":", round(joined_zim$g2_edu_dropout_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Education Dropout")  %>%  
    addPolygons(fillColor = ~mypal(joined_zim$g2_hea_chronic_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name, ":", round(joined_zim$g2_hea_chronic_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Chronic Ilness") %>%
    addPolygons(fillColor = ~mypal(joined_zim$g2_hea_visit_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name, ":", round(joined_zim$g2_hea_visit_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Health Visit") %>%
    addPolygons(fillColor = ~mypal(joined_zim$g2_assets_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name, ":", round(joined_zim$g2_assets_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Household Assets") %>%
    addPolygons(fillColor = ~mypal(joined_zim$g2_services_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name, ":", round(joined_zim$g2_services_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Service Access") %>%
    
    addPolylines(data = joined_zim$geometry, color = "black", opacity = 2, weight = 2,)%>% 
    setView(lat = -19.0154, lng=29.1549 , zoom =6) %>% 
    addLegend(pal = mypal,position = "bottomright",values = joined_zim$g2_edu_max_k3,
              opacity = .6,title= paste("MPI Component Value")) %>% 
    addLayersControl(baseGroups = c("No Primary Education","Education Dropout", "Chronic Ilness","Health Visit", "Household Assets","Service Access"), 
                     options = layersControlOptions(collapsed = FALSE), position = "topright") %>%
    #hideGroup("M0")%>% 
    hideGroup("Education Dropout")%>% 
    hideGroup("Chronic Ilness") %>% 
    hideGroup("Health Visit") %>% 
    hideGroup("Household Assets") %>% 
    hideGroup("Service Access")
})




output$compo_MPI_17 <- renderLeaflet({
  mypal <- colorNumeric(
    palette = "viridis",
    domain = NULL,
    reverse = TRUE)
  
  leaflet(joined_zim17) %>% addTiles() %>%  
    addPolygons(fillColor = ~mypal(g0_edu_max_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name, ":", round(joined_zim17$g0_edu_max_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="No Primary Education") %>%
    addPolygons(fillColor = ~mypal(joined_zim17$g0_edu_dropout_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name, ":", round(joined_zim17$g0_edu_dropout_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Education Dropout")  %>%  
    addPolygons(fillColor = ~mypal(joined_zim17$g0_hea_chronic_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name, ":", round(joined_zim17$g0_hea_chronic_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Chronic Ilness") %>%
    addPolygons(fillColor = ~mypal(joined_zim17$g0_hea_visit_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name, ":", round(joined_zim17$g0_hea_visit_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Health Visit") %>%
    addPolygons(fillColor = ~mypal(joined_zim17$g0_assets_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name, ":", round(joined_zim17$g0_assets_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Household Assets") %>%
    addPolygons(fillColor = ~mypal(joined_zim17$g0_services_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name, ":", round(joined_zim17$g0_services_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Service Access") %>%
    
    addPolylines(data = joined_zim17$geometry, color = "black", opacity = 2, weight = 2,)%>% 
    setView(lat = -19.0154, lng=29.1549 , zoom =6) %>% 
    addLegend(pal = mypal,position = "bottomright",values = joined_zim17$g0_edu_max_k3,
              opacity = .6,title= paste("MPI Component Value")) %>% 
    addLayersControl(baseGroups = c("No Primary Education","Education Dropout", "Chronic Ilness","Health Visit", "Household Assets","Service Access"), 
                     options = layersControlOptions(collapsed = FALSE), position = "topright") %>%
    #hideGroup("M0")%>% 
    hideGroup("Education Dropout")%>% 
    hideGroup("Chronic Ilness") %>%
    hideGroup("Health Visit") %>% 
    hideGroup("Household Assets") %>% 
    hideGroup("Service Access")
  
})

#M1
output$compo_MPI_17_m1 <- renderLeaflet({
  mypal <- colorNumeric(
    palette = "viridis",
    domain = NULL,
    reverse = TRUE)
  
  leaflet(joined_zim17) %>% addTiles() %>%  
    addPolygons(fillColor = ~mypal(g1_edu_max_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name, ":", round(joined_zim17$g1_edu_max_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="No Primary Education") %>%
    addPolygons(fillColor = ~mypal(joined_zim17$g1_edu_dropout_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name, ":", round(joined_zim17$g1_edu_dropout_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Education Dropout")  %>%  
    addPolygons(fillColor = ~mypal(joined_zim17$g1_hea_chronic_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name, ":", round(joined_zim17$g1_hea_chronic_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Chronic Ilness") %>%
    addPolygons(fillColor = ~mypal(joined_zim17$g1_hea_visit_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name, ":", round(joined_zim17$g1_hea_visit_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Health Visit") %>%
    addPolygons(fillColor = ~mypal(joined_zim17$g1_assets_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name, ":", round(joined_zim17$g1_assets_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Household Assets") %>%
    addPolygons(fillColor = ~mypal(joined_zim17$g1_services_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name.x, ":", round(joined_zim17$g1_services_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Service Access") %>%
    
    addPolylines(data = joined_zim17$geometry, color = "black", opacity = 2, weight = 2,)%>% 
    setView(lat = -19.0154, lng=29.1549 , zoom =6) %>% 
    addLegend(pal = mypal,position = "bottomright",values = joined_zim17$g1_edu_max_k3,
              opacity = .6,title= paste("MPI Component Value")) %>% 
    addLayersControl(baseGroups = c("No Primary Education","Education Dropout", "Chronic Ilness","Health Visit", "Household Assets","Service Access"), 
                     options = layersControlOptions(collapsed = FALSE), position = "topright") %>%
    #hideGroup("M0")%>% 
    hideGroup("Education Dropout")%>% 
    hideGroup("Chronic Ilness") %>%
    hideGroup("Health Visit") %>% 
    hideGroup("Household Assets") %>% 
    hideGroup("Service Access")
  
})


#M2
output$compo_MPI_17_m2 <- renderLeaflet({
  mypal <- colorNumeric(
    palette = "viridis",
    domain = NULL,
    reverse = TRUE)
  
  leaflet(joined_zim17) %>% addTiles() %>%  
    addPolygons(fillColor = ~mypal(g2_edu_max_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name, ":", round(joined_zim17$g2_edu_max_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="No Primary Education") %>%
    addPolygons(fillColor = ~mypal(joined_zim17$g2_edu_dropout_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name, ":", round(joined_zim17$g2_edu_dropout_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Education Dropout")  %>%  
    addPolygons(fillColor = ~mypal(joined_zim17$g2_hea_chronic_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name, ":", round(joined_zim17$g2_hea_chronic_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Chronic Ilness") %>%
    addPolygons(fillColor = ~mypal(joined_zim17$g2_hea_visit_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name, ":", round(joined_zim17$g2_hea_visit_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Health Visit") %>%
    addPolygons(fillColor = ~mypal(joined_zim17$g2_assets_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name, ":", round(joined_zim17$g2_assets_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Household Assets") %>%
    addPolygons(fillColor = ~mypal(joined_zim17$g2_services_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name.x, ":", round(joined_zim17$g2_services_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Service Access") %>%
    
    addPolylines(data = joined_zim17$geometry, color = "black", opacity = 2, weight = 2,)%>% 
    setView(lat = -19.0154, lng=29.1549 , zoom =6) %>% 
    addLegend(pal = mypal,position = "bottomright",values = joined_zim17$g2_edu_max_k3,
              opacity = .6,title= paste("MPI Component Value")) %>% 
    addLayersControl(baseGroups = c("No Primary Education","Education Dropout", "Chronic Ilness","Health Visit", "Household Assets","Service Access"), 
                     options = layersControlOptions(collapsed = FALSE), position = "topright") %>%
    #hideGroup("M0")%>% 
    hideGroup("Education Dropout")%>% 
    hideGroup("Chronic Ilness") %>%
    hideGroup("Health Visit") %>% 
    hideGroup("Household Assets") %>% 
    hideGroup("Service Access")
  
})







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

output$my_slick5 <- renderSlickR(
  slickR(
    my_images5,
    width = "90%"
  )
)

output$my_slick_corr <- renderSlickR(
  slickR(
    my_images6,
    width = "90%"
  )
)
output$my_slick7 <- renderSlickR(
  slickR(
    my_images7,
    width = "60%"
  )
)


output$my_slick8 <- renderSlickR(
  slickR(
    my_images8,
    width = "60%"
  )
)

output$my_slick9 <- renderSlickR(
  slickR(
    my_images9,
    width = "60%"
  )
)


}

# Run the application ----------------
shinyApp(ui = ui, server = server)
