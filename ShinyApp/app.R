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
#gpclibPermit()

## FORMATTING-------------------------------------------------------------------
prettyblue <- "#232D4B"
navBarBlue <- '#427EDC'
options(spinner.color = prettyblue, spinner.color.background = '#ffffff', spinner.size = 3, spinner.type = 7)

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

# CODE TO DETECT ORIGIN OF LINK AND CHANGE LOGO ACCORDINGLY
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
  domain = NULL)


#EVI DATA
#agregion
GrSs2011 <- read_csv("./data/agregion indices/evi/EVI_region_GrSs2011.csv")
GrSs2017 <- read_csv("./data/agregion indices/evi/EVI_region_GrSs2017.csv")
EVI_region_long <- read_csv("./data/agregion indices/evi/EVI_region_long.csv")

#PRECIPITATION DATA

#SOIL DATA
mydatXL2 <- read_csv(paste0(getwd(),"/data/agregion indices/soil/soil_ts_Zimb.csv"))
mydat_long <- readRDS("./data/agregion indices/soil/mydat_long.RDS")
#df2 <- read.csv("C:/Users/Leo Allen/Downloads/soil_hist.csv")
df2 <- readRDS("./data/agregion indices/soil/soil_hist.RDS")
total <- readRDS("./data/agregion indices/soil/soil_map.RDS")



#MPI DATA
MPI_2011 <- read_excel("./data/MPI/2011_MPI_w_components.xlsx")
MPI_2017 <- read_excel("./data/MPI/2017_MPI_w_components.xlsx")






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
                                   h1(strong("Using Remotely Sensed Data for Social & Economic Decision Making in Zimbabwe")),
                                   # fluidRow(style = "margin: 2px;",
                                   #          img(src = "Zimbabwe_Flag.png", height="100", width="200", alt="Image", style="display: block; margin-left: auto; margin-right: auto; border: 1px solid #000000;")),
                                   h4("Data Science for the Public Good Program"),
                                   h4("Virginia Tech"),
                                   h4("Department of Agricultural and Applied Economics")
                                   
                          ),
                          
                          fluidRow(style = "margin: 6px;",
                                   column(4,
                                          h2(strong("Project Overview"), align = "center"),
                                          p("In Zimbabwe, agriculture is a mainstay of the economy and livelihood for most rural poor. Zimbabwe has experienced increased social and economic unrest since 2000, with macroeconomic instability and diseases contributing to the problem. Extreme droughts in 2003 and 2016 contributed to increased food insecurity and a significant increase in rural poverty. Additionally, an ill-conceived fast-track land reform beginning in 2000 led to the decapitalization of the commercial agriculture sector."),
                                          p("In this project, we identify the publicly available remotely sensed climate-related data available and suitable for Zimbabwe. These are the Enhanced Vegetation Index, Precipitation, and Soil Moisture. We use these indices to provide a geospatial analysis of the five agro-ecological regions in the 2010-11 and 2016-17 growing seasons. And we analyze the climatic conditions ideal for maize, the primary crop grown in Zimbabwe. We disaggregate our analysis to the 60 administrative district-level to study the association between poverty and climate indicators. To do this, we augment the climate data with poverty variables constructed from the national Poverty, Income, Consumption, Expenditure Survey (PICES) conducted in 2011 and 2017."),
                                          p("We then use these data in a statistical model to examine the association between district-level poverty and climatic conditions. The Zimbabwean government has recently approved an agricultural policy framework based on climate-smart principles. Still, it contains little geographic specificity in an incredibly diverse agricultural economy. Our analysis provides a spatially disaggregated look at whether climate data can be used to identify at-risk regions for potential policy intervention.")),
                                   
                                   
                                   column(4,
                                          h2(strong("Introduction to Zimbabwe"), align = "center"),
                                          p("Nestled in the Southeastern tip of Africa, Zimbabwe neighbors South Africa, Mozambique, Zambia, and Botswana. Zimbabwe gained independence from Great Britain in 1980 and was ruled by Prime Minister and eventually President Robert Mugabe until his resignation in 2017.
                                            After gaining independence in 1980, there was widespread hope that the economic and labor exploitation Africans suffered at the hands of an imperial Great Britain would diminish. 
                                            While initial trends were encouraging, this hope dwindled as many factors sent the Zimbabwean economy into decline. Most prominent among these factors was the central government’s inconsistent policy, 
                                            which resulted in vague and evolving strategies for combatting poverty."),
                                            p("A scientific socialist policy approach was applied between 1980 and 1990 to address poverty but was ineffective and abandoned due to a 
                                            financial downturn coupled with a prolonged drought which forced agricultural workers into the cities where they faced even greater poverty due to unemployment. In an attempt to revamp the economy, 
                                            Zimbabwe sought help from the International Monetary Fund (IMF) and the World Bank (WB), which led to adopting a different approach to economic development. The costs of necessities, including food, water, and education, 
                                            went up, harming and expanding the already existing poor population. The late 1990s and 2000s brought greater poverty and financial distress to Zimbabwe ever."), 
                                            p("A continuing government budget deficit mixed with a fiscal policy focused on increasing the amount of money in circulation which resulted in hyperinflation. 
                                            In turn, this increased the economic crisis as foreign investment dropped and Zimbabwean currency crashed. During this time, unemployment skyrocketed, and a massive informal sector of the economy emerged. 
                                            In 2009, Zimbabwe adopted the US dollar along with a handful of other currencies. Though this move somewhat stabilized the economy initially, a 2013 shift in government rendered these efforts futile. 
                                            By 2017, inflation increased significantly, as did the overall economic crisis and poverty."), 
                                            p("Presently, Emmerson Mnangagwa holds office. The country is home to roughly 14,830,000 inhabitants, 10% of whom live in the capital city of Harare. Although large agglomerations exist in other major urban areas, including Bulawayo and Chitungwiza, the population distribution is relatively evenly dispersed throughout the country otherwise. Zimbabwe’s central government is responsible for regulating its ten provinces, and 59 further subdivided districts. Zimbabwe’s terrain consists mainly of a flat plateau upon which forests thrive."), 
                                          p("Arable land is plenty, and  67.5 percent of the labor force works in agriculture growing sugar cane, tobacco, fruit, and vegetables, among other things. Another 7.3% of the labor force takes advantage of Zimbabwe’s rich natural resources and participates in mining. Zimbabwe exports coal, gold, platinum, copper, and other metals and manufacturing wood products, cement, chemicals, fertilizer, and food. Despite being relatively well-educated and highly literate, the population suffers from both unemployment and severe underemployment. Many individuals are either overqualified for the jobs they have or are engaging in full-time work. Together with low wages, this creates an obstacle to economic growth."),
                                          p("Monetary poverty measures in 2017 revealed that roughly 63% of Zimbabwean households lived in poverty. Together with the high poverty rate, the country experiences income inequality, malnourishment, low life expectancy, high infant/maternal mortality rates, difficulty accessing health and education resources, and overall low living standards.")),
                                   
                                   column(4,
                                          h2(strong("Agricultural Profile"), align = "center"),
                                          p("89% of farmers in Zimbabwe are smallholders who rely on rain for their agriculture and less than 1% of whom have access to irrigation (Milne, Mekonnen, & Benitez Ponce, 2019). This limited access to water affects food production and leads to food insecurity issues, especially during droughts which are exacerbated by irrigation and water storage infrastructure being in a state of disrepair (Milne, Mekonnen, & Benitez Ponce, 2019). Climate change is anticipated to have a negative impact on agriculture due to more floods and droughts in addition to changing temperatures and precipitation patterns. Without adequate adaptation a drier climate is projected to decrease GDP due to agriculture sector losses by 2 percent (Milne, Mekonnen, & Benitez Ponce, 2019)."),
                                          h3(em("Regional Specificity"), align = "center"),
                                          img(src = "AgroRegionZim.png", style = "display: inline; border: 0px solid #C0C0C0; margin-left: auto; margin-right: auto;", width = "110%"),
                                          div(tags$caption("Figure 1: Agro-ecological regions of Zimbabwe")),
                                          p(""),
                                          p("Depending on the region, certain crops are more suited than others with regions I, II, and III being better suited to producing commercial crops due to their better rainfall patterns, while regions IV and V are better suited to livestock farming and irrigated agriculture (Milne, Mekonnen, & Benitez Ponce, 2019). To be more specific, Region I is suitable for fruit, forestry, and intensive livestock production; Region II can grow maize, cotton, flue cured tobacco, sugar, beans, and coffee and grows sorghum, seed maize, barley, groundnuts, and various horticultural crops as well; Region III is mostly used for extensive beef ranching and commercial farm production is primarily consisted of Maize; while regions IV and V require irrigation for successful crop production due to their dryness, communal farmers must grow crops without access to irrigation anyway, with Millet and sorghum being the most common crops and maize being grown as well"),
                                          
                                          )
                                   
                                   
                                   
                                   
                          ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: August 2022'))))
                 ),
                 
                 ## Tab data and methodology ----------------------------------------------------
                 navbarMenu(strong("Data & Methodology"), 
                            
                            tabPanel(strong("Data"), 
                                     fluidPage(#h1(strong("Data"), align = "center"),
                                       box(
                                         width = 6,
                                       h2(strong("Description of the Remote Sensed Data")),
                                       withMathJax(),
                                       h3(strong("EVI")),
                                       p("Description of EVI"),
                                       
                                       h3(strong("Precipitation")),
                                       p("TRMM 3B42 is a Google Earth Engine (GEE) indicator to observe and record all forms of tropical precipitation such as snow, rain, drizzle, & etc. The dataset is provided by NASA GES DISC at NASA Goddard Space Flight Center. It has undergone processing through their TMPA (TRMM Multi-satellite Precipitation Analysis) Algorithm in which merged high quality (HQ)/infrared (IR) precipitation and root-mean-square (RMS) precipitation-error estimates results in a dataset. The data is produced over a 3-hour period and rendered at a resolution of 27830 meters (about 17.29 mi) observed around the global belt (50° North and South). The unit of measurement provided is in millimeters per hour (TRMM, 2012)."),
                                       
                                       h3(strong("Soil Moisture")),
                                       p("Our data set, the NASA-USDA Enhanced SMAP Global soil moisture data, provides global soil moisture information at a 10km spatial resolution and includes five indices: Surface and Subsurface soil moisture, Soil moisture profile (percent soil moisture), and surface and subsurface soil moisture anomalies from 2015 to 2022. The dataset is derived by taking predictions from the modified Palmer two-layer model which are then corrected through the integration of satellite derived Soil Moisture Active Passive (SMAP) soil moisture observations (Bolten, Sazib, & Mladenova, 2021). The integration of the SMAP imagery into the Palmer model is done using an Ensemble Kalman Filter (EnKF) method, and is designed specifically to correct model-based predictions for damaging impacts due to rainfall-forcing errors; especially for parts of the world without exhaustive rain gauge instrumentation (Bolten, Sazib, & Mladenova, 2018c). This is of great importance as the quality of the assimilation greatly depends on the accuracy of observational and model estimates, meaning that proper evaluation of the soil moisture uncertainty is vital for the best integration of the satellite observations (Maggioni, Anagnostou, & Reichle, 2012)."),
                                       ),
                                       
                                      # br(),
                                       
                                       box(
                                         width = 6,
                                       h2(strong("Description of the PICES DATA")),
                                       withMathJax(),  
                                       p("The data come from two nationally representative household surveys, called the PICES, conducted by ZIMSTAT: first, from June 2011 to May 2012, and second, from January to December 2017. The PICES surveys are well suited to construct multidimensional poverty indices because they include information at the household and individual levels, and they are collected repeatedly. The surveys were conducted in the eight provinces of Zimbabwe and in the cities of Harare and Bulawayo. The number of usable observations (households) is 29,748 in 2011–2012 (23,843 rural and 5,905 urban) and 31,193 in 2017 (25,525 rural and 5668 urban). Survey weights and household size are employed to obtain national, provincial, and rural-urban representation. Both survey instruments are virtually identical across the two waves. They include information on household demographics, education, employment, healthcare, migration, housing characteristics, assets ownership, access to services, and agricultural activities."),
                                       h3(strong("Description of the Variables/Components")),
                                       img(src = "variables.png", style = "display: inline; border: 0px solid #C0C0C0; margin-left: auto; margin-right: auto;", width = "100%"),
                                       withMathJax(), 
                                       p("To construct the multidimensional poverty index based on the Alkire-Foster method, we consider eight   poverty dimensions consisting of 14 variables relevant to identifying poverty status. The first dimension, education, consists of two variables – Max Education and Education Dropout. The Max Education variable refers to nobody in the household having completed primary school. We assess the sensitivity of the MPI by broadening these measures to nobody in the household having completed secondary school. The Education Dropout variable is an indicator variable for whether the household has a child aged 7-11 who is not enrolled in school. The education dimension receives the greatest weight in the MPI (2 out of 9.5), along with the two health variables that make up the second health dimension (2 out of 9.5). These two variables are Chronic Illness, referring to the presence of a chronically ill individual within the household, and Lack of Health Visit, which refers to a household member who has been sick in the past 30 days without receiving a necessary healthcare."),
                                       p("Unemployment, defined as one member of the household having been unemployed as their main occupation in the last 12 months, is given a weight of 1 for urban households and 0 for rural households since unemployment is less common and is more difficult to identify in rural areas.  "),
                                       p("For housing conditions, two variables are considered: lack of access to electricity and no toilet (in rural areas) or no flush toilet (for urban areas with more developed sanitation). Weights of 0.5 are given to rural residence Lack of Electricity and Lack of Toilet indicators underlying the dimension. In urban areas, where lack of electricity indicates a greater state of deprivation, a weight of one is attributed to electricity. In contrast, the lack of a toilet retains a weight of 0.5."),
                                       p("Two variables reflect living conditions: Poor Water Source and Poor Cooking Fuel, with a weight of 0.5 for each. Rural households are considered to be deprived if their main water source is an unprotected well, a river, or another unprotected source, or if the water source is 1 km away or farther. In urban areas with more developed water infrastructure, deprivation is defined as not having access to piped water or communal water on-premises (which affects only a small number of households). In rural and urban areas, households are deprived if they use wood or ’other’ (not electricity, paraffin, gas, coal) as cooking fuel.  "),
                                       p("Lack of Household Assets is given a dimension weight of 1 in both rural and urban areas. The stock of household assets is measured by a physical asset index (PAI) and an asset deprivation (D) threshold as follows:  "),
                                       
                                       
                                       p(" \\(PAI = 2 * motor vehicle + motorcycle + bicycle + television + radio + fridge + landline phone\\)   ", align = "center"),
                                       p("\\( D = 1 \\)", " if ","\\(PAI < 2 \\)", align = "center"),
                                       p("For rural households, agricultural assets are essential indicators of wellbeing and agricultural activity capabilities. The dimension weight is 1.5, with three component variables usually given a weight of 0.5 each. The first variable, Lack of Land, uses a threshold of 0.25 hectares, which is sufficiently low to represent effective deprivation of landholding in rural Zimbabwe. The second variable on livestock is measured in Tropical Livestock Units (TLU), an indicator of wealth that can be used to insulate households from negative idiosyncratic and covariateshocks. A TLU deprivation threshold of 1 indicates  Lack of Livestock. The third variable is the Lack of Rural Equipment. An agricultural equipment index (AEI) is created as follows:  "),
                                       p("\\( AEI = plough + wheelbarrow + scotchcart + tractor + griding mill \\)", align = "center"),
                                       p("\\( D = 1 \\)", " if ","\\(AEI < 1 \\)", align = "center"),
                                       p("The agricultural asset dimension is not included for households in urban areas.  "),
                                       p("The final dimension of wellbeing – with a weight of 1 – is Lack of Access to Services, where remoteness indicates deprivation. Households are considered deprived if they are far from two or more of seven recorded services in the data. The distance thresholds employed are 5 km for a primary school, 15 km for a secondary school, 15 km for a hospital, 5 km for shops, 6 km for a hammer mill, 15 km for a post office, and 5 km for a bus stop, respectively. These distance thresholds are halved in urban areas, where services tend to be closer, but distance still represents a barrier to access."),
                                       p("**Note: The livestock data were not available in the 2011-12 wave, which limited our ability to compare the change in livestock dimension across time. To account for this, we have assigned the Lack of Livestock variable a weight of zero and divided the weight proportionally between the other two agricultural asset variables. We use this adjusted index to compare the MPI for 2011 and 2017."))
                                     )
                            ),
                            
                            
                            tabPanel(strong("Methodology"), 
                                     fluidRow(
                                       box(
                                         withMathJax(),
                                         title = h3(strong("Remote Sensed data Methodology")),
                                         width = 6,
                                         em(h4("A brief overview of the wrangling of the Remote Sensed data")), tags$br(),
                                         h3(strong("EVI")),
                                         p("Description of EVI"),
                                         h3(strong("Precipitation")),
                                         p("In this project, we subset the data to investigate over Zimbabwe and within a timeframe that parallels PICES data records from 2011 and 2017. As well as making connection with other events with our timeframe such as severe drought episodes and social/economic shocks. We decided to limit the timeframe of the indicator due to the 3-hourly characteristic of the data and then aggregate the data to daily averages of precipitation."),
                                         p("Zimbabwe’s agriculture sector is the economic backbone that provides livelihoods to its people. With respect to this, we have decided to transition the spatial mapping of Zimbabwe to an agro-ecological region level to accurately depict precipitation in meaningful way. Additionally, to further narrow our examination of the country we decided to focus on the production of a single primary crop, maize. Zimbabwe generally follows previous analysis of its weather pattern but as it relates to precipitation the Northern regions are typically the ones to receive the most rain and the opposite is true for the Southern regions (Nkomozepi & Chung, 2012). Past literature defines the growing season of maize to start in October and continue into May the following year (Nyakudya & Stroosnijder, 2011)."),
                                         p("To produce maize, a season must see 600-700 mm of rain (Mugyio et al, 2021). If a certain season receives a maximum of 1000 mm of rain, then yields may increase. However, yields may decline if the total rainfall exceeds 1000 mm for a season (Mushore et al., 2016).  In regions with rainfed agriculture and rain patterns becoming more variable as the years progress the timing of the most optimal planting period in a season can dictate the success of a season’s yield. Planting prematurely can lead to crop failure; this may be due to an extensive dry spell mid-season."),
                                         p("Alternatively, planting late can reduce the growing season, considering both situations the yield will be reduced as a by-product (Mugyio et al., 2021). In a day, we need to characterize the possible types of conditions that may affect the production of maize. For this project, we will only examine what is a dry or wet day. A day is determined to be wet if the precipitation value is less than 2.95mm. The opposite can describe a wet day as receiving more than 2.95mm of rain (Mugyio et al., 2021). A derived measurement to indicate a dry spell is important in understanding the performance of the growing seasons. A dry spell consists of 14 consecutive dry days or more (Mupangwa et al., 2011; Mugiyo et al., 2021)."),
                                         h3(strong("Soil Moisture")),
                                         p("Surface soil moisture is the water that is in the upper 10cm of soil and responds quickly to heavy precipitation and rapidly drying events (Drought.gov, 2022). For our dataset, the surface soil moisture is assumed to hold a maximum of one inch of water meaning the top layer soil depth varies based on soil texture. Appropriate Surface soil moisture levels are necessary for the success of planting and harvesting activities for most crops with too little soil moisture during planting stifling the seed germination and too much soil moisture preventing fieldwork or heavy machinery access to the field (Bolten et al., 2018c). To be specific, soil moisture levels of:"),
                                        #br(), 
p("-   20-25mm are best for the germination and emergence of a new crop but can halt fieldwork or damage a newly seeded crop that is in the wet environment for a prolonged period."),

p("-   15-20mm are normally the best for vigorous field activity."),

p("-   10mm or less will not support the early growth potential for a newly emerged crop or seed germination (Bolten et al., 2018c)."),
                                       ),
                                       box(
                                         withMathJax(),
                                         title = h3(strong("MPI Methodology")),
                                         width = 6,
                                         em(h4("A brief overview of the Mathematics behind the Multidimensional Poverty Index")), tags$br(),
                                         p("The aggregate methodology for determining the multidimensional poverty 
       indices proposed by Alkine and Foster in 2011 involve a matrix with \\(n\\) 
       rows and \\(d\\) columns, where \\(n\\) is the number of people within the 
       state and \\(d\\) is the number of dimensions for assessing poverty. There 
       are three main measurements denoted on the \\(M\\) scale: \\(M_{0}, M_{1}\\) and \\(M_{2}\\).
       The A-F method employed in this study contains eight dimensions of poverty. 
       Within each dimension, there are one or two variables that indicate whether 
       an individual is deprived in that area. Each variable has a specific
       weight associated with it depending on its contribution to overall poverty
       and how it pertains to rural and urban communities differently. For a given 
       individual, the total number of deprivations are added up and if he or she falls
       above a given threshold, \\(k\\), then that individual is considered poor. 
       Having multiple dimensions of poverty allows us to decompose the original 
       measure into its individual variables to identify which are contributing 
       most to the overall index of poverty."),
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
                                         h5(em("\\(k\\) = Threshold (If an index is above threshold, k, then the individual is considered poor)")),
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
                                         
                                       )
                                     )
                            )),
                            
                            
                            
                            tabPanel(strong("Resources"), 
                                     fluidPage(
                                       column(4,
                                              h3(strong("Google Earth Engine")),
                                              img(src = "data-google-earth.png", style = "display: inline; float: left;", width = "140px"),
                                              withMathJax(),  
                                              p("Google Earth Engine combines a multi-petabyte catalog of satellite imagery and geospatial datasets with planetary-scale analysis capabilities and makes it available for scientists, researchers, and developers to detect changes, map trends, and quantify differences on the Earth's surface. We used it to collect data on NDVI, EVI, precipitation and Soil moisture in Zimbabwe.")),
                                       
                                       column(4,
                                              h3(strong("Google Maps")),
                                              img(src = "data-gmaps.png", style = "display: inline; float: left;", width = "140px"),
                                              withMathJax(), 
                                              p("Google Maps is a comprehensive web mapping service created by Google. Its goal is to provide an interactive map of all the geographical contents of the world. This resource has a variety of uses, ranging from examining all service locations within a city to finding the quickest route between locations. It provides data at latitude and longitude level. We used Google Maps to visualize weather information behind the Google Earth Engine.")),
                                       
                                       column(4,
                                              h3(strong("ZimStat")),
                                              img(src = "zimstat_logo.png", style = "display: inline; float: left;", width = "140px"),
                                              withMathJax(), 
                                              p("Zimbabwe National Statistics Agency (ZimStat) is a corporate body established through the Census and Statistics Act of 2007 and the main source of official statistics in Zimbabwe We used the the national Poverty, Income, Consumption, Expenditure Survey (PICES) conducted in 2011 and 2017."))
                                       
                                       )),
                            
                            
                            
                            
                 ),
                 
                 
                 ## Tab X Data-----------------------
                 tabPanel(strong("Data & Methodology"),
                          tabsetPanel(
                            tabPanel(strong("Data"),
                                    
                                     fluidRow(
                                       box(
                                         width = 6,
                                         withMathJax(),
                                         title = h1(strong("Remote Sensed Data")),
                                         h4(em("Description of the Remote Sensed Data")),
                                         withMathJax(),
                                         h3(strong("EVI")),
                                         p("Description of EVI"),
                                         
                                         h3(strong("Precipitation")),
                                         p("TRMM 3B42 is a Google Earth Engine (GEE) indicator to observe and record all forms of tropical precipitation such as snow, rain, drizzle, & etc. The dataset is provided by NASA GES DISC at NASA Goddard Space Flight Center. It has undergone processing through their TMPA (TRMM Multi-satellite Precipitation Analysis) Algorithm in which merged high quality (HQ)/infrared (IR) precipitation and root-mean-square (RMS) precipitation-error estimates results in a dataset. The data is produced over a 3-hour period and rendered at a resolution of 27830 meters (about 17.29 mi) observed around the global belt (50° North and South). The unit of measurement provided is in millimeters per hour (TRMM, 2012)."),
                                         
                                         h3(strong("Soil Moisture")),
                                         p("Our data set, the NASA-USDA Enhanced SMAP Global soil moisture data, provides global soil moisture information at a 10km spatial resolution and includes five indices: Surface and Subsurface soil moisture, Soil moisture profile (percent soil moisture), and surface and subsurface soil moisture anomalies from 2015 to 2022. The dataset is derived by taking predictions from the modified Palmer two-layer model which are then corrected through the integration of satellite derived Soil Moisture Active Passive (SMAP) soil moisture observations (Bolten, Sazib, & Mladenova, 2021). The integration of the SMAP imagery into the Palmer model is done using an Ensemble Kalman Filter (EnKF) method, and is designed specifically to correct model-based predictions for damaging impacts due to rainfall-forcing errors; especially for parts of the world without exhaustive rain gauge instrumentation (Bolten, Sazib, & Mladenova, 2018c). This is of great importance as the quality of the assimilation greatly depends on the accuracy of observational and model estimates, meaning that proper evaluation of the soil moisture uncertainty is vital for the best integration of the satellite observations (Maggioni, Anagnostou, & Reichle, 2012)."),
                                         
                                         br()),
                                         
                                         box(
                                           
                                           width = 6,
                                           withMathJax(),
                                           title = h1(strong("PICES Data")),
                                           p("The data come from two nationally representative household surveys, called the PICES, conducted by ZIMSTAT: first, from June 2011 to May 2012, and second, from January to December 2017. The PICES surveys are well suited to construct multidimensional poverty indices because they include information at the household and individual levels, and they are collected repeatedly. The surveys were conducted in the eight provinces of Zimbabwe and in the cities of Harare and Bulawayo. The number of usable observations (households) is 29,748 in 2011–2012 (23,843 rural and 5,905 urban) and 31,193 in 2017 (25,525 rural and 5668 urban). Survey weights and household size are employed to obtain national, provincial, and rural-urban representation. Both survey instruments are virtually identical across the two waves. They include information on household demographics, education, employment, healthcare, migration, housing characteristics, assets ownership, access to services, and agricultural activities."),
                                           h3(strong("Description of the Variables/Components")),
                                           img(src = "variables.png", style = "display: inline; border: 0px solid #C0C0C0; margin-left: auto; margin-right: auto;", width = "100%"),
                                           withMathJax(), 
                                           p("To construct the multidimensional poverty index based on the Alkire-Foster method, we consider eight   poverty dimensions consisting of 14 variables relevant to identifying poverty status. The first dimension, education, consists of two variables – Max Education and Education Dropout. The Max Education variable refers to nobody in the household having completed primary school. We assess the sensitivity of the MPI by broadening these measures to nobody in the household having completed secondary school. The Education Dropout variable is an indicator variable for whether the household has a child aged 7-11 who is not enrolled in school. The education dimension receives the greatest weight in the MPI (2 out of 9.5), along with the two health variables that make up the second health dimension (2 out of 9.5). These two variables are Chronic Illness, referring to the presence of a chronically ill individual within the household, and Lack of Health Visit, which refers to a household member who has been sick in the past 30 days without receiving a necessary healthcare."),
                                           p("Unemployment, defined as one member of the household having been unemployed as their main occupation in the last 12 months, is given a weight of 1 for urban households and 0 for rural households since unemployment is less common and is more difficult to identify in rural areas.  "),
                                           p("For housing conditions, two variables are considered: lack of access to electricity and no toilet (in rural areas) or no flush toilet (for urban areas with more developed sanitation). Weights of 0.5 are given to rural residence Lack of Electricity and Lack of Toilet indicators underlying the dimension. In urban areas, where lack of electricity indicates a greater state of deprivation, a weight of one is attributed to electricity. In contrast, the lack of a toilet retains a weight of 0.5."),
                                           p("Two variables reflect living conditions: Poor Water Source and Poor Cooking Fuel, with a weight of 0.5 for each. Rural households are considered to be deprived if their main water source is an unprotected well, a river, or another unprotected source, or if the water source is 1 km away or farther. In urban areas with more developed water infrastructure, deprivation is defined as not having access to piped water or communal water on-premises (which affects only a small number of households). In rural and urban areas, households are deprived if they use wood or ’other’ (not electricity, paraffin, gas, coal) as cooking fuel.  ")
                                           
                                           
                                           ))),
                            
                            tabPanel(strong("Methodology"),
                                     
                                     # tabName = "91_Dist",
                                     # # Everything has to be put in a row or column
                                     fluidRow(
                                       box(
                                         withMathJax(),
                                         title = h3(strong("Remote Sensed data Methodology")),
                                         width = 6,
                                         em(h4("A brief overview of the wrangling of the Remote Sensed data")),
                                         h4(strong("Enhanced Vegetation Index")),
                                         p("text"),
                                         
                                         h4(strong("Precipitation")),
                                         p("In this project, we subset the data to investigate over Zimbabwe and within a timeframe that parallels PICES data records from 2011 and 2017. As well as making connection with other events with our timeframe such as severe drought episodes and social/economic shocks. We decided to limit the timeframe of the indicator due to the 3-hourly characteristic of the data and then aggregate the data to daily averages of precipitation."),
                                         p("Zimbabwe’s agriculture sector is the economic backbone that provides livelihoods to its people. With respect to this, we have decided to transition the spatial mapping of Zimbabwe to an agro-ecological region level to accurately depict precipitation in meaningful way. Additionally, to further narrow our examination of the country we decided to focus on the production of a single primary crop, maize. Zimbabwe generally follows previous analysis of its weather pattern but as it relates to precipitation the Northern regions are typically the ones to receive the most rain and the opposite is true for the Southern regions (Nkomozepi & Chung, 2012). Past literature defines the growing season of maize to start in October and continue into May the following year (Nyakudya & Stroosnijder, 2011)."),
                                         p("To produce maize, a season must see 600-700 mm of rain (Mugyio et al, 2021). If a certain season receives a maximum of 1000 mm of rain, then yields may increase. However, yields may decline if the total rainfall exceeds 1000 mm for a season (Mushore et al., 2016).  In regions with rainfed agriculture and rain patterns becoming more variable as the years progress the timing of the most optimal planting period in a season can dictate the success of a season’s yield. Planting prematurely can lead to crop failure; this may be due to an extensive dry spell mid-season."),
                                         p("Alternatively, planting late can reduce the growing season, considering both situations the yield will be reduced as a by-product (Mugyio et al., 2021). In a day, we need to characterize the possible types of conditions that may affect the production of maize. For this project, we will only examine what is a dry or wet day. A day is determined to be wet if the precipitation value is less than 2.95mm. The opposite can describe a wet day as receiving more than 2.95mm of rain (Mugyio et al., 2021). A derived measurement to indicate a dry spell is important in understanding the performance of the growing seasons. A dry spell consists of 14 consecutive dry days or more (Mupangwa et al., 2011; Mugiyo et al., 2021)."),
                                         
                                         h4(strong("Soil Moisture")),
                                         p("Surface soil moisture is the water that is in the upper 10cm of soil and responds quickly to heavy precipitation and rapidly drying events (Drought.gov, 2022).

For our dataset, the surface soil moisture is assumed to hold a maximum of one inch of water meaning the top layer soil depth varies based on soil texture. Appropriate Surface soil moisture levels are necessary for the success of planting and harvesting activities for most crops with too little soil moisture during planting stifling the seed germination and too much soil moisture preventing fieldwork or heavy machinery access to the field (Bolten et al., 2018c). To be specific, soil moisture levels of:"),
                                        #br(), 
p("-   20-25mm are best for the germination and emergence of a new crop but can halt fieldwork or damage a newly seeded crop that is in the wet environment for a prolonged period."),

p("-   15-20mm are normally the best for vigorous field activity."),

p("-   10mm or less will not support the early growth potential for a newly emerged crop or seed germination (Bolten et al., 2018c)."),
                                         tags$br(),
                                       ),
                                       box(
                                         withMathJax(),
                                         title = h3(strong("MPI Methodology")),
                                         width = 6,
                                         em(h4("A brief overview of the Mathematics behind the Multidimensional Poverty Index")), tags$br(),
                                         p("The aggregate methodology for determining the multidimensional poverty 
       indices proposed by Alkine and Foster in 2011 involve a matrix with \\(n\\) 
       rows and \\(d\\) columns, where \\(n\\) is the number of people within the 
       state and \\(d\\) is the number of dimensions for assessing poverty. There 
       are three main measurements denoted on the \\(M\\) scale: \\(M_{0}, M_{1}\\) and \\(M_{2}\\).
       The A-F method employed in this study contains eight dimensions of poverty. 
       Within each dimension, there are one or two variables that indicate whether 
       an individual is deprived in that area. Each variable has a specific
       weight associated with it depending on its contribution to overall poverty
       and how it pertains to rural and urban communities differently. For a given 
       individual, the total number of deprivations are added up and if he or she falls
       above a given threshold, \\(k\\), then that individual is considered poor. 
       Having multiple dimensions of poverty allows us to decompose the original 
       measure into its individual variables to identify which are contributing 
       most to the overall index of poverty."),
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
                                         h5(em("\\(k\\) = Threshold (If an index is above threshold, k, then the individual is considered poor)")),
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
                                       column(4,
                                              h3(strong("Google Earth Engine")),
                                              img(src = "data-google-earth.png", style = "display: inline; float: left;", width = "140px"),
                                              withMathJax(),  
                                              p("Google Earth Engine combines a multi-petabyte catalog of satellite imagery and geospatial datasets with planetary-scale analysis capabilities and makes it available for scientists, researchers, and developers to detect changes, map trends, and quantify differences on the Earth's surface. We used it to collect data on NDVI, EVI, precipitation and Soil moisture in Zimbabwe.")),
                                       
                                       column(4,
                                              h3(strong("Google Maps")),
                                              img(src = "data-gmaps.png", style = "display: inline; float: left;", width = "140px"),
                                              withMathJax(), 
                                              p("Google Maps is a comprehensive web mapping service created by Google. Its goal is to provide an interactive map of all the geographical contents of the world. This resource has a variety of uses, ranging from examining all service locations within a city to finding the quickest route between locations. It provides data at latitude and longitude level. We used Google Maps to visualize weather information behind the Google Earth Engine.")),
                                       
                                       column(4,
                                              h3(strong("ZimStat")),
                                              img(src = "zimstat_logo.png", style = "display: inline; float: left;", width = "140px"),
                                              withMathJax(), 
                                              p("Zimbabwe National Statistics Agency (ZimStat) is a corporate body established through the Census and Statistics Act of 2007 and the main source of official statistics in Zimbabwe We used the the national Poverty, Income, Consumption, Expenditure Survey (PICES) conducted in 2011 and 2017."))
                                       
                                     ))
                          )),
                 
                 

                 ## Tab 1---------------------
                 navbarMenu(strong("Remote Sensed Data"), 
                            tabPanel(strong("Enhanced Vegetation Index"),
                                     
                                     # tabName = "91_Dist",
                                     # # Everything has to be put in a row or column
                                     fluidRow(
                                       box(withSpinner(plotOutput("myplot")),
                                         title = "Enhanced Vegetation Index (EVI)",
                                         width = 6,
                                         height = 600
                                       ),
                                         box(
                                           
                                           width = 4,
                                           withMathJax(),
                                           title = "Description",
                                           p("This graphic shows a detailed visualization of the Enhanced Vegetation Index for the Zimbabwean districts and broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. There are three layers to this graph:")))),
                            
                            tabPanel(strong("Precipitation (Rainfall)"),
                                     
                                     # tabName = "91_Dist",
                                     # # Everything has to be put in a row or column
                                     fluidRow(
                                       box(withSpinner(plotOutput("myplot2")),
                                         title = "Precipitation (Rainfall)",
                                         width = 6,
                                         height = 600
                                         ),
                                         
                                         box(
                                           
                                           width = 4,
                                           withMathJax(),
                                           title = "Description",
                                           p("This graphic shows a detailed visualization of the Precipitation for the Zimbabwean districts and broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. There are three layers to this graph:")))),
                            
                            
                            tabPanel(strong("Soil Moisture"),
                                     tabPanel("Surface Soil Moisture",
                                              fluidRow(style = "margin: 6px;",
                                                       h1(strong("Surface Soil Moisture "), align = "center"),
                                                       p("", style = "padding-top:10px;"),
                                                       
                                                       column(6,
                                                              h4(strong("Why Soil Moisture?")),
                                                              p("Appropriate Surface soil moisture levels are necessary for the success of planting and harvesting activities for most crops with too little soil moisture during planting stifling the seed germination and too much soil moisture preventing fieldwork or heavy machinery access to the field (Bolten, Sazib, & Mladenova, 2018). Because most planting activities take place during the first 30 days of the growing season, this is the time period we have chosen to focus on for the Surface soil moisture section of our study.")
                                                              ),
                                                       
                                                       column(6,
                                                              h4(strong("Ideal soil type for maize production")),
                                                              p("The germination of maize seeds is dependent to a large extent on soil and environmental condition with warm, moist conditions resulting in seedling emergence of 6 to 10 days, while cool or dry conditions slowing emergence to two weeks or longer. The optimum moisture levels of the soil is approximately 60% of the total capacity while optimum soil texture is between 10-30% clay content. Maize grows best in fertile, deep, well-drained soils where total annual rainfall is greater than 500mm. Maize is susceptible to both drought and water logging and therefore poorly drained soils should be avoided. Furthermore, drought during silking and tasseling, which occurs during the four-week period spanning flowering, can lead to high yield losses and resultingly some form of water conservation is beneficial."))
                                              )),
                                     fluidRow(
                                       box(withSpinner(leafletOutput("soil_map_leaflet", height=520)),
                                           title = "Average Soil Moisture",
                                           width = 7
                                           #height = 600
                                       ),
                                       box(
                                         width = 4,
                                         withMathJax(),
                                         title = "Description",
                                         p("This visualization shows the average surface soil moisture (in mm) by Zimbabwe’s natural regions. The average is taken over the first 30 days of the 2016-17 growing season, which takes place from November 20th to December 20th of 2016. From the visualization we can see that regions I, IIa, IIb, and III have dry surface soil moisture (10-15mm), while regions IV and V have extremely dry surface soil moisture (>10mm). These soil moisture levels suggest that while farmers in all regions of Zimbabwe are likely to experience stifled germination upon planting during the 2016/2017 growing season, farmers in regions IV and V are likely to be more impacted than their counterparts in the other regions."))),
                                     
#                                      fluidRow(
#                                        box(withSpinner(plotOutput("soil_map")),
#                                          title = "Average Soil Moisture",
#                                          width = 8,
#                                          height = 600
#                                        ),
#                                          box(
#                                            width = 4,
#                                            withMathJax(),
#                                            title = "Description",
#                                            p("This graphic shows a detailed visualization of the soil moisture for the Zimbabwean districts and broken up # into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. There are three layers to this graph:"))),
                                    
                                     #fluidRow(
                                       box(withSpinner(plotOutput("soil_hist")),
                                           title = "Soil Moisture At Planting",
                                           width = 8
                                           #height = 600
                                       ),
                                       box(
                                         width = 4,
                                         withMathJax(),
                                         title = "Description",
                                         p("This grouped bar chart shows the number of 3-day periods by region that fall within each of the four soil condition categories. The number of three-day periods is taken over the first 30 days of the 2016-17 growing season, which takes place from November 20th to December 20th of 2016. From this visualization we can see that none of the regions experienced any wet days, and region V is unique in not experiencing any ideal days. Furthermore, Regions one through three all had either 4 or 5 ideal days, while region four only had 2. This aligns with the previous visualization’s findings of regions I-III having more soil moisture on average than regions IV and V.")),
                                       
                                     #fluidRow(  
                                     box(withSpinner(plotOutput("soil_line")),
                                           title = "Soil Moisture at Planting Times",
                                           width = 8
                                           #height = 600
                                       ),
                                       box(
                                         width = 4,
                                         withMathJax(),
                                         title = "Description",
                                         p("This line chart shows by region the surface soil moisture in mm over the first 30 days of the 2016-17 growing season, which takes place from November 20th to December 20th of 2016. From this visualization we can see that the ranking of soil moisture levels by region remains largely consistent over the time period, the difference between the region with the highest soil moisture and the region with the lowest roughly doubles over the first 30 days of the growing season. In addition, while regions I – III experience soil moisture levels above the extremely dry threshold (10mm) as early as November 24th*, regions IV and V do not reach those levels until December 9th*."))
                                       
                                       
                                      )),
                            
                            
                            #),
                 
                 ## Tab 2------
                 navbarMenu(strong("Multidimensional Poverty Index (MPI)"), 
                            tabPanel(strong("Multidimensional Poverty Index"),
                                     
                                     tabsetPanel(
                                     tabPanel(title = "2011",
                                     fluidRow(
                                       box(withSpinner(leafletOutput("MPI_map_2011", height=520)),
                                         title = "Multidimensional Poverty Index",
                                         width = 8,
                                         height = 600
                                       ),
                                         box(
                                           
                                           width = 4,
                                           withMathJax(),
                                           title = "Description",
                                           p("This graphic shows a detailed visualization of Zimbabwean districts, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES, the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:
                                      \\(M_{0}\\), \\(M_{1}\\), and \\(M_{2}\\)."), 
                                           tags$ul(  
                                             tags$li("\\(M_{0}\\) is the ",strong("adjusted headcount ratio")," designed by",a(href="https://ophi.org.uk/research/multidimensional-poverty/alkire-foster-method/","Sabina Alkire and James Foster",target="_blank"),
                                                     " and considers all of the dimensions described in the methodology section."),
                                             tags$li("\\(M_{1}\\)
                                      is the ",strong("adjusted poverty gap")," an index to show how far below the poor people are from the poverty line."),
                                             tags$li("\\(M_{2}\\) is the ",strong("square of the adjusted poverty gap."),"By squaring the poverty gaps, this measure puts a higher weight on those who are farther away from the poverty line. Thus, this index measures severity of poverty.")
                                             
                                           ),
                                           p("To adjust the threshold cutoff, k, by which an individual is considered poor,
                                      we use k=3 as our threshold."))),
                                     
                                     fluidRow(
                                       box(
                                         withMathJax(),
                                         title = strong("Descriptive Analysis"),
                                         width = 12,
                                         p("\\(M_{0}\\)"),
                                         p("Looking at the original poverty index and focusing on the \\(M_{0}\\) index, we can see that for low k-threshold values, a large portion of the population can be considered multidimensionally poor. Additionally, urban districts and urban households tend to have lower \\(M_{0}\\) scores than their rural counterparts. As we increase the k-threshold values, thereby increasing the criteria to be labeled multidimensionally poor, fewer people across the country can be identified as such. The greater Harare and Bulawayo areas have low \\(M_{0}\\) values for low k-thresholds. Still, their \\(M_{0}\\) values for higher k-thresholds are above the national average, implying that while those districts are better on average, some of the most poverty-stricken households reside within their bounds (particularly the Epworth district)."),
                                         
                                         p("\\(M_{1}\\)"),
                                         p("When we focus on the depth of poverty (\\(M_{1}\\) index ), if the k-thresholdvalues are low, poverty throughout much of Zimbabwe can be considered deep.  A majority of \\(M_{1}\\) values exceed the national \\(M_{1}\\) value. Similar to the \\(M_{0}\\) trends, urban districts tend to have lower \\(M_{1}\\) values than rural districts, implying deeper poverty in rural districts. Although the number of districts portraying deep poverty generally decreases as k-threshold values increase, this is not the case for rural districts neighboring Harare, including Bindura, Goromonzi, and Marondera. These areas maintain high \\(M_{1}\\) values as k-threshold values increase, as do a cluster of districts in the country’s southeastern region."),
                                         p("\\(M_{2}\\)"),
                                         p("A look at the \\(M_{2}\\) values of the original index reveals much of the same. Low k-threshold values render high rates of poverty severity across a large proportion of Zimbabwe’s population. As k-threshold values increase, \\(M_{2}\\) values fall throughout most of the country but remain substantially high in the western portion of the country and around Harare, implying a greater number of impoverished households are further away from the poverty line than other impoverished households in these regions. If we distinguish between urban and rural, we can see that urban districts tend to have less severe poverty than rural districts, excluding the urban aggregates in Umguza, Bubi, and Mutasa. "),
                                         p("")
                                       )
                                     )),
                                     
                                     tabPanel(title = "2017",
                                              fluidRow(
                                                box(withSpinner(leafletOutput("MPI_map_2017", height=520)),
                                                    title = "Multidimensional Poverty Index",
                                                    width = 8,
                                                    height = 600
                                                ),
                                                box(
                                                  
                                                  width = 4,
                                                  withMathJax(),
                                                  title = "Description",
                                                  p("This graphic shows a detailed visualization of Zimbabwean districts, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES, the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:
                                      \\(M_{0}\\), \\(M_{1}\\), and \\(M_{2}\\)."), 
                                                  tags$ul(  
                                                    tags$li("\\(M_{0}\\) is the ",strong("adjusted headcount ratio")," designed by",a(href="https://ophi.org.uk/research/multidimensional-poverty/alkire-foster-method/","Sabina Alkire and James Foster",target="_blank"),
                                                            " and considers all of the dimensions described in the methodology section."),
                                                    tags$li("\\(M_{1}\\)
                                      is the ",strong("adjusted poverty gap")," an index to show how far below the poor people are from the poverty line."),
                                                    tags$li("\\(M_{2}\\) is the ",strong("square of the adjusted poverty gap."),"By squaring the poverty gaps, this measure puts a higher weight on those who are farther away from the poverty line. Thus, this index measures severity of poverty.")
                                                    
                                                  ),
                                                  p("To adjust the threshold cutoff, k, by which an individual is considered poor,
                                      we use k=3 as our threshold."))),
                                              
                                              fluidRow(
                                                box(
                                                  withMathJax(),
                                                  title = strong("Descriptive Analysis"),
                                                  width = 12,
                                                  p("\\(M_{0}\\)"),
                                                  p("Looking at the original poverty index and focusing on the \\(M_{0}\\) index, we can see that for low k-threshold values, a large portion of the population can be considered multidimensionally poor. Additionally, urban districts and urban households tend to have lower \\(M_{0}\\) scores than their rural counterparts. As we increase the k-threshold values, thereby increasing the criteria to be labeled multidimensionally poor, fewer people across the country can be identified as such. The greater Harare and Bulawayo areas have low \\(M_{0}\\) values for low k-thresholds. Still, their \\(M_{0}\\) values for higher k-thresholds are above the national average, implying that while those districts are better on average, some of the most poverty-stricken households reside within their bounds (particularly the Epworth district)."),
                                                  
                                                  p("\\(M_{1}\\)"),
                                                  p("When we focus on the depth of poverty (\\(M_{1}\\) index ), if the k-thresholdvalues are low, poverty throughout much of Zimbabwe can be considered deep.  A majority of \\(M_{1}\\) values exceed the national \\(M_{1}\\) value. Similar to the \\(M_{0}\\) trends, urban districts tend to have lower \\(M_{1}\\) values than rural districts, implying deeper poverty in rural districts. Although the number of districts portraying deep poverty generally decreases as k-threshold values increase, this is not the case for rural districts neighboring Harare, including Bindura, Goromonzi, and Marondera. These areas maintain high \\(M_{1}\\) values as k-threshold values increase, as do a cluster of districts in the country’s southeastern region."),
                                                  p("\\(M_{2}\\)"),
                                                  p("A look at the \\(M_{2}\\) values of the original index reveals much of the same. Low k-threshold values render high rates of poverty severity across a large proportion of Zimbabwe’s population. As k-threshold values increase, \\(M_{2}\\) values fall throughout most of the country but remain substantially high in the western portion of the country and around Harare, implying a greater number of impoverished households are further away from the poverty line than other impoverished households in these regions. If we distinguish between urban and rural, we can see that urban districts tend to have less severe poverty than rural districts, excluding the urban aggregates in Umguza, Bubi, and Mutasa. "),
                                                  p("")
                                                )
                                              ),
                                     
                                     ))),
                            
                            
                            
                            tabPanel(strong("Components of the MPI"),
                                     
                                     tabsetPanel(
                                       tabPanel(title = "2011",
                                     fluidRow(
                                       box(withSpinner(leafletOutput("compo_MPI_11", height = 520)),
                                         title = "Components of the MPI",
                                         width = 8,
                                         height = 600
                                       ),
                                         box(
                                           
                                           width = 4,
                                           withMathJax(),
                                           title = "Description",
                                           p("This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES, the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:")))),
                                     
                                     tabPanel(title = "2017",
                                              fluidRow(
                                                box(withSpinner(leafletOutput("compo_MPI_17", height = 520)),
                                                    title = "Components of the MPI",
                                                    width = 8,
                                                    height = 600
                                                ),
                                                box(
                                                  
                                                  width = 4,
                                                  withMathJax(),
                                                  title = "Description",
                                                  p("This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES, the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:"))))
                                     
                                     )),
                            
                            
                            tabPanel(strong("Tab 3"),
                                     
                                     # tabName = "91_Dist",
                                     # # Everything has to be put in a row or column
                                     fluidRow(
                                       box(withSpinner(plotOutput("myplot4", height = 520)),
                                         title = "Tab 3",
                                         
                                         box(
                                           
                                           width = 4,
                                           withMathJax(),
                                           title = "Description",
                                           p("This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES, the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:")))))),
                            
                            
                 
                 ## Tab 3
                 navbarMenu(strong("MPI and Indices"), 
                            tabPanel(strong("MPI & EVI"),
                                     
                                     # tabName = "91_Dist",
                                     # # Everything has to be put in a row or column
                                     fluidRow(
                                       box(
                                         title = "MPI & EVI",
                                         
                                         box(
                                           
                                           width = 4,
                                           withMathJax(),
                                           title = "Description",
                                           p("This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES, the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:"))))),
                            
                            tabPanel(strong("MPI & Precipitation"),
                                     
                                     # tabName = "91_Dist",
                                     # # Everything has to be put in a row or column
                                     fluidRow(
                                       box(
                                         title = "MPI & Precipitation",
                                         
                                         box(
                                           
                                           width = 4,
                                           withMathJax(),
                                           title = "Description",
                                           p("This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES, the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:"))))),
                            
                            
                            tabPanel(strong("MPI & Soil Moisture"),
                                     
                                     # tabName = "91_Dist",
                                     # # Everything has to be put in a row or column
                                     fluidRow(
                                       box(
                                         title = "MPI & Soil Moisture",
                                         
                                         box(
                                           
                                           width = 4,
                                           withMathJax(),
                                           title = "Description",
                                           p("This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES, the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:"))))),
                            
                            
                 ),
                 

                  ## Tab Takeaways --------------

                 tabPanel(strong("Takeaways"),
                          
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   column(3),
                                   column(6,
                                          h1(strong("Takeaways"),align="center"),
                                          p("The analysis presented here provides an interactive way to present remote sensed data and a multidimensional poverty index along different components. We display the remote sensed data: Enhanced Vegetation Index, Precipitation, and Soil Moisture from the Google Earth Engine and  the MPIs in maps. We allow users to assess the remote sensed data by district and the agroecological regions in Zimbabwe. We allow users to explore the  decomposed MPIs into selected components, allowing users to look at the poverty indices of the individual components and their link to the remote sensed data. Finally, we offer users the ability to view the explore changes between the two most recent waves of PICES surveys (2011 & 2017). "),
                                          p("From our analyses, we see that EVI... "),
                                          p("We note that precipitation ..."),
                                          p("We note that Soil Moisture ..."),
                                          p("We note that MPI ..."),
                                          p("Finally,"),
                                   )
                                   
                          )
                          
                 ),
                 
                 
                 
                 
                 ## Tab DSPG Team------------------------------------------------
                 tabPanel(strong("Our Team"), 
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   h1(strong("Project Team"), align = "center"),
                                   br(),
                                   h4(strong("VT Data Science for the Public Good"), align = "center"),
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
                                   column(6, align = "center",
                                          h4(strong("DSPG Team Members")),
                                          p("", style = "padding-top:10px;"),
                                          img(src = "team-Leo.png", style = "display: inline;  border: 0px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-Poonam.png", style = "display: inline; border: 0px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-Frankie.png", style = "display: inline; border: 0px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-Ari.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-Josue.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-Josue.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p("", style = "padding-top:10px;"),
                                          p(a(href = 'https://aaec.vt.edu/people/graduatestudents/index/quaye-leonard-allen.html', 'Leonard-Allen Quaye', target = '_blank'), "(Virginia Tech, Agricultural and Applied Microeconomics, PHD);"),
                                          p(a(href = 'https://www.bse.vt.edu/people/grad-students/poonam-tajanpure.html', 'Poonam Tajanpure', target = '_blank'), "(Virginia Tech, BSE PHD);"),
                                          p(a(href = 'https://www.linkedin.com/in/frankie-ruoyu-fan/?lipi=urn%3Ali%3Apage%3Ad_flagship3_people_connections%3BBiz9W9pbRcO00B0bou%2F2vg%3D%3D', 'Frankie Fan', target = '_blank'), "(Virginia Tech, Computational Modeling and Data Analytics);"),
                                          p(a(href = 'https://www.linkedin.com/in/ari-l-12b151123/?lipi=urn%3Ali%3Apage%3Ad_flagship3_people_connections%3B5WMwWerMTvefiu%2Fq85Z5mw%3D%3D', 'Ari Liverpool', target = '_blank'), "(Virginia Tech, Applied Economics Major, Data and Decisions minor);"),
                                          p( a(href = 'https://www.linkedin.com/in/josue-navarrete-36a6321b4/?lipi=urn%3Ali%3Apage%3Ad_flagship3_people_connections%3B5WMwWerMTvefiu%2Fq85Z5mw%3D%3D', 'Josue Navarrete', target = '_blank'), "(Virginia Tech, Philosophy, Politics, & Economics)."),
                                          p( a(href = 'https://www.linkedin.com/in/naveen-abedin-0ab1089a/?lipi=urn%3Ali%3Apage%3Ad_flagship3_people_connections%3BgdZR16ktRcatg1cpCMufuQ%3D%3D', 'Naveen Abedin', target = '_blank'), "(Virginia Tech, Philosophy, Politics, & Economics)."),
                                          p("", style = "padding-top:10px;")
                                          
                                   ),
                                   column(6, align = "center",
                                          h4(strong("Virginia Tech Faculty Members")),
                                          p("", style = "padding-top:10px;"),
                                          img(src = "faculty-posadas.jpg", style = "display: inline; border: 0px solid #C0C0C0;", width = "150px"),
                                          img(src = "faculty-chen.jpg", style = "display: inline;  border: 0px solid #C0C0C0;", width = "150px"),
                                          img(src = "faculty-alwang.jpg", style = "display: inline; border: 0px solid #C0C0C0;", width = "150px"),
                                          p("", style = "padding-top:10px;"),
                                          p(a(href = "https://www.linkedin.com/in/briannaposadas/", 'Dr. Brianna Posadas', target = '_blank'), "(Virginia Tech, School of Plant and Environmental Sciences);"),
                                          p(a(href = "https://aaec.vt.edu/people/faculty/chen-susan.html", 'Dr. Susan Chen', target = '_blank'), "(Virginia Tech, Agricultural and Applied Microeconomics);"),
                                          p(a(href = "https://aaec.vt.edu/people/faculty/alwang-jeffrey.html", 'Dr. Jeffrey Alwang', target = '_blank'), "(Virginia Tech, Agricultural and Applied Microeconomics)."),
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
                                   p("We would like to thank ",a(href="https://www.researchgate.net/profile/Tawanda-Chingozha","Tawanda Chingoza",target='_blank')," of Stellenbosch University for providing us with consultative insights on Zimbabwe and support on this project;"),
                                   p(a(href="https://tw.linkedin.com/in/kuo-hao-lai","Kuo-Hao Lai",target='_blank')," (Virginia Tech); "),
                                   p(a(href="http://www.uwyo.edu/wygisc/people/yang_di/di-short-cv.html","Dr. Di Yang",target='_blank')," (Wyoming Geographic Information Science Center - WyGISC); "),
                                   p("We also thank ZimStat for providing 2011 and 2017 PICES data for this project.")
                                   
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
                                 p("TRMM. (2012). Nasa.gov. https://trmm.gsfc.nasa.gov/3b42.html "),
                                 p("Reference"),
                                 p("Reference"),
                                 p("Reference")

                                 
                                 
                                 
                                 
                                 
                                 )
                 ),
                 inverse = T)




## Define server logic required---------------------
server <- function(input, output) {
  # Run JavaScript Code
  runjs(jscode)
  
output$myplot <- renderPlot({
    ggplot(mtcars, aes(disp, mpg)) + geom_point()
      #ggtitle(sprintf("%s's plot in %s", input$name, input$state))
  })
  
output$myplot2 <- renderPlot({
    ggplot(mtcars, aes(disp, mpg)) + geom_point()
    #ggtitle(sprintf("%s's plot in %s", input$name, input$state))
  })
  
  
output$myplot3 <- renderPlot({
  ggplot(mtcars, aes(disp, mpg)) + geom_point()
  #ggtitle(sprintf("%s's plot in %s", input$name, input$state))
})

output$myplot4 <- renderPlot({
  ggplot(mtcars, aes(disp, mpg)) + geom_point()
  #ggtitle(sprintf("%s's plot in %s", input$name, input$state))
})


# EVI OUTPUTS-------



# SOIL MOISTURE OUTPUTS-------
output$soil_map_leaflet <- renderLeaflet({
  mypal <- colorNumeric(
    palette = "viridis",
    domain = total$AvgSurfaceMoisture)
  
  leaflet(total) %>% addTiles() %>%  
    addPolygons(color = ~mypal(AvgSurfaceMoisture), weight = 1, smoothFactor = 0.5, label = paste("Region -", total$region,":", round(total$AvgSurfaceMoisture, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE)) %>% 
    addLegend(pal = mypal,position = "bottomright",values = total$AvgSurfaceMoisture,
              opacity = .6,title= paste("Average Soil Moisture (mm)"))%>%
    
    addPolylines(data = total$geometry, color = "black", opacity = 2, weight = 2,)%>% 
    setView(lat = -19.0154, lng=29.1549 , zoom =6)
})

output$soil_hist <- renderPlot({
  # Grouped
  ggplot(df2, aes(fill=time, y=value, x=region)) + 
    geom_bar(position="dodge", stat="identity")+ 
    labs(color="time") +
    xlab("Agro-ecological Region") + ylab("Number Of 3-Day Periods") + 
    ggtitle("Soil Moisture Conditions In The Planting Time During The 2016-17 Growing Season") +
    guides(fill=guide_legend(title="Soil Condition")) + labs(caption = "3 Day: NASA-USDA Enhanced SMAP Global") +
    scale_fill_viridis(discrete=TRUE, direction=-1)
  #3day periods within 30 days of 11/19/16 by region and Surf-soil moisture condition  
})  

output$soil_line <- renderPlot({
  
  #Generate categories for soil moisture
  mydat_longg <- mydat_long %>% 
    mutate(
      Wet = case_when(Moisture > 25 ~ 1,
                      TRUE ~ 0),
      
      Ideal = case_when(Moisture >= 15 & Moisture <=25 ~ 1,
                        TRUE ~ 0),
      
      Dry = case_when(Moisture >10 & Moisture <15 ~ 1,
                      TRUE ~ 0),
      
      ExtremelyDry = case_when(Moisture <=10 ~ 1,
                               TRUE ~ 0),
    )
  
  # Make sure that the data is in columns for each region. This step will group by column to get the desired wide dataset.
  #Put into columns by group
  mydat_long2 <- transform(mydat_longg,                                 # Create ID by group
                           ID = as.numeric(factor(region))) %>% select(-c(region)) %>% reshape(idvar = "newDate", 
                                                                                               timevar = "ID", direction = "wide")
  # Now let us look at the first month of the growing season. This means we will subset our sample to the first 30 days of planting in the 2016-17 Growing Season. We set this with the min and max variables below.
  # Set limits to first thirty days of the growing season: c(min, max)
  min <- as.yearmon("20161119", "%Y%m")
  max <- as.yearmon("20161219", "%Y%m")
  
  # Set axis limits c(min, max) on plot
  min <- as.Date("2016-11-19")
  max <- as.Date("2016-12-19")
  
ggplot(mydat_long2, aes(newDate, y = value, color = variable)) + 
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
  


# #Add maps
# output$soil_map <- renderPlot({
#   ggplot(data = total) +
#     geom_sf(size = 0.15, color = "black", aes(fill = AvgSurfaceMoisture)) +
#     xlab("Longitude") + ylab("Latitude") +
#     coord_sf() +
#     scale_fill_viridis(option = "viridis", direction = -1, limits=c(6,14), breaks=c(6,8,10,12,14), labels=c("6","8", "10", "12", "14")) +
#     ggtitle("Average Soil Moisture During The First 30 days Of 2016-17 Growing Season") + labs(caption = "3 day: NASA-USDA Enhanced SMAP Global") +
#     guides(fill=guide_legend(title="Average Soil Moisture (mm)"))+theme_bw()
# })


#MPI OUTPUTS -----
output$MPI_map_2011 <- renderLeaflet({
  leaflet(joined_zim) %>% addTiles() %>%  
    addPolygons(color = ~mypal(M0_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name.x, ":", round(joined_zim$M0_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="M0") %>%
    addPolygons(color = ~mypal(M1_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name.x, ":", round(joined_zim$M1_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="M1")  %>%  
    addPolygons(color = ~mypal(M2_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name.x, ":", round(joined_zim$M2_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="M2") %>%
    addPolylines(data = joined_zim$geometry, color = "black", opacity = 2, weight = 2,)%>% 
    setView(lat = -19.0154, lng=29.1549 , zoom =6) %>% 
    addLegend(pal = mypal,position = "bottomleft",values = joined_zim$M0_k3,
              opacity = .6,title= paste("MPI 2011")) %>% 
    addLayersControl(baseGroups = c("M0", "M1", "M2"), 
                     options = layersControlOptions(collapsed = FALSE), position = "topright") %>%
    #hideGroup("M0")%>% 
    hideGroup("M1")%>% 
    hideGroup("M2")
})

output$MPI_map_2017 <- renderLeaflet({

  leaflet(joined_zim17) %>% addTiles() %>%  
    addPolygons(color = ~mypal(M0_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name.x, ":", round(joined_zim$M0_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="M0") %>%
    addPolygons(color = ~mypal(M1_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name.x, ":", round(joined_zim$M1_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="M1")  %>%  
    addPolygons(color = ~mypal(M2_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name.x, ":", round(joined_zim$M2_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="M2") %>%
    addPolylines(data = joined_zim$geometry, color = "black", opacity = 2, weight = 2,)%>% 
    setView(lat = -19.0154, lng=29.1549 , zoom =6) %>% 
    addLegend(pal = mypal,position = "bottomleft",values = joined_zim$M0_k3,
              opacity = .6,title= paste("MPI 2017")) %>% 
    addLayersControl(baseGroups = c("M0", "M1", "M2"), 
                     options = layersControlOptions(collapsed = FALSE), position = "topright") %>%
    #hideGroup("M0")%>% 
    hideGroup("M1")%>% 
    hideGroup("M2")
})

output$compo_MPI_11 <- renderLeaflet({
  leaflet(joined_zim) %>% addTiles() %>%  
    addPolygons(fillColor = ~mypal(g0_edu_max_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name.x, ":", round(joined_zim$g0_edu_max_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Max Education") %>%
    addPolygons(fillColor = ~mypal(joined_zim$g0_edu_dropout_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name.x, ":", round(joined_zim$g0_edu_dropout_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Education Dropout")  %>%  
    addPolygons(fillColor = ~mypal(joined_zim$g0_hea_chronic_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim$District_name.x, ":", round(joined_zim$g0_hea_chronic_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Chronic Ilness") %>%
    addPolylines(data = joined_zim$geometry, color = "black", opacity = 2, weight = 2,)%>% 
    setView(lat = -19.0154, lng=29.1549 , zoom =6) %>% 
    addLegend(pal = mypal,position = "bottomleft",values = joined_zim$g0_edu_max_k3,
              opacity = .6,title= paste("k=3")) %>% 
    addLayersControl(baseGroups = c("Max Education","Education Dropout", "Chronic Ilness"), 
                     options = layersControlOptions(collapsed = FALSE), position = "topright") %>%
    #hideGroup("M0")%>% 
    hideGroup("Education Dropout")%>% 
    hideGroup("Chronic Ilness")
})

output$compo_MPI_17 <- renderLeaflet({
  mypal <- colorNumeric(
    palette = "viridis",
    domain = NULL)
  
  leaflet(joined_zim17) %>% addTiles() %>%  
    addPolygons(fillColor = ~mypal(g0_edu_max_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name.x, ":", round(joined_zim17$g0_edu_max_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Max Education") %>%
    addPolygons(fillColor = ~mypal(joined_zim17$g0_edu_dropout_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name.x, ":", round(joined_zim17$g0_edu_dropout_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Education Dropout")  %>%  
    addPolygons(fillColor = ~mypal(joined_zim17$g0_hea_chronic_k3), weight = 1, smoothFactor = 0.5, label = paste("", joined_zim17$District_name.x, ":", round(joined_zim17$g0_hea_chronic_k3, digits = 3)),
                opacity = 1.0, fillOpacity = 0.5,
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE), group="Chronic Ilness") %>%
    addPolylines(data = joined_zim17$geometry, color = "black", opacity = 2, weight = 2,)%>% 
    setView(lat = -19.0154, lng=29.1549 , zoom =6) %>% 
    addLegend(pal = mypal,position = "bottomleft",values = joined_zim17$g0_edu_max_k3,
              opacity = .6,title= paste("k=3")) %>% 
    addLayersControl(baseGroups = c("Max Education","Education Dropout", "Chronic Ilness"), 
                     options = layersControlOptions(collapsed = FALSE), position = "topright") %>%
    #hideGroup("M0")%>% 
    hideGroup("Education Dropout")%>% 
    hideGroup("Chronic Ilness")
  
  
})


}

# Run the application ----------------
shinyApp(ui = ui, server = server)
