#Leo's setting WD
# setwd("C:/Users/Leo Allen/Desktop/DSPG/2022_DSPG_Zimbabwe/Shiny Test/Zim22)

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
zim_district <- st_read("./data/Shapefiles/Zim_D60.shp")  
zim_region <- st_read("./data/Shapefiles/agro-ecological-regions.shp")

#EVI DATA
EVI_monthly <- read_csv("./data/EVI_monthly.csv")
AnnualEVI <- read_csv("./data/EVI_annual.csv")
EVI_long <- read_csv("./data/EVI_long.csv") 


#PRECIPITATION DATA


#SOIL DATA



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
                                          p("In Zimbabwe, agriculture is a mainstay of the economy and the source of livelihoods for the majority of rural poor. Zimbabwe has experienced increased social and economic unrest since 2000, with a series of drought, macro-economic instability, and diseases contributing to the problem. Additionally, an ill-conceived fast-track land reform beginning in 2000 led to decapitalization of the commercial agriculture sector, while extreme droughts in 2003 and 2016 contributed to increased food insecurity and a huge increase in rural poverty."),
                                          p("Prior research suggests that poverty in Zimbabwe has increased since the period of crisis began at the turn of the millennium. According to the latest World Bank (2020) estimates, due to the longstanding economic crisis and disruptions following the COVID-19 pandemic, 49% of Zimbabwe’s population was in extreme poverty in 2020. Zimbabwe’s government seeks guidance in policies to enhance the climate resilience of its agricultural sector and contribute to sustainable enhancement in rural conditions. It has made available to researchers a trove of household survey data from large national samples for 2000, 2011, 2017 and 2019 with the hope that these can be used to inform agricultural policy."),
                                          p("This project seeks to:", strong("(i) to identify the different remotely sensed climate/weather related data available for Zimbabwe; (ii) to use these data to construct a spatial profile of exposure to long-term climate changes and short-term adverse weather events; and (iii) analyze the benefit of these remotely sensed data to explain demographic conditions."), 
                                            p("In doing so, the project seeks to contribute to understanding the impacts of a climate-resilient agricultural policy. The Zimbabwean government has recently approved an agricultural policy framework based on climate-smart principles, but it contains very little geographic specificity in an incredibly diverse agricultural economy. "),
                                          p("This project uses data from the Poverty, Income, Consumption, Expenditure Survey (PICES) to provide granular information on poverty in Zimbabwe. We created multidimensional poverty indices (MPI) at the", strong("district and province level"), " and decomposed them into components that focus on ", strong("education, health, employment, housing conditions, living conditions, assets, agricultural assets, and access to services."), "Additionally, we obtain data on our selected indices: Enhanced Vegetation Index, Precipitation, and Soil Moisture from the Google Earth Engine."),   
                                            "We provide interactive tools that allow the user to visualize and study each remote sensed data index and understand their contribution to the MPI and the components of the MPI. We constructed these measures for two waves of data, 2011 and 2017, to show the changes in weather over time and across the districts and the agroecological regions. The overall goal of this project is to understand how remote sensing data can be used to inform socioeconomic and climate policy. So, we seek to understand how remotely sensed data can be used to inform climate change and poverty in Zimbabwe.")),
                                   
                                   
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
                                     fluidPage(
                                       h2(strong("Description of the Remote Sensed Data")),
                                       withMathJax(),
                                       h3(strong("EVI")),
                                       p("Description of EVI"),
                                       
                                       h3(strong("Precipitaion")),
                                       p("Description of Precipitation"),
                                       
                                       h3(strong("Soil Moisture")),
                                       p("Description of Soil Moisture"),
                                       
                                       br(),
                                       
                                       h2(strong("Description of the PICES DATA")),
                                       withMathJax(),  
                                       p("The data come from two nationally representative household surveys, called the PICES, conducted by ZIMSTAT: first, from June 2011 to May 2012, and second, from January to December 2017. The PICES surveys are well suited to construct multidimensional poverty indices because they include information at the household and individual levels, and they are collected repeatedly. The surveys were conducted in the eight provinces of Zimbabwe and in the cities of Harare and Bulawayo. The number of usable observations (households) is 29,748 in 2011–2012 (23,843 rural and 5,905 urban) and 31,193 in 2017 (25,525 rural and 5668 urban). Survey weights and household size are employed to obtain national, provincial, and rural-urban representation. Both survey instruments are virtually identical across the two waves. They include information on household demographics, education, employment, healthcare, migration, housing characteristics, assets ownership, access to services, and agricultural activities."),
                                       h3(strong("Description of the Variables/Components")),
                                       img(src = "variables.png", style = "display: inline; border: 0px solid #C0C0C0; margin-left: auto; margin-right: auto;", width = "80%"),
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
                                       p("**Note: The livestock data were not available in the 2011-12 wave, which limited our ability to compare the change in livestock dimension across time. To account for this, we have assigned the Lack of Livestock variable a weight of zero and divided the weight proportionally between the other two agricultural asset variables. We use this adjusted index to compare the MPI for 2011 and 2017."),
                                       h3(strong("Heading")),
                                       p("text")
                                     )
                                     
                                     
                                     
                                     
                            ),
                            tabPanel(strong("Methodology"), 
                                     fluidPage(
                                       box(
                                         withMathJax(),
                                         title = h3(strong("MPI Methodology")),
                                         width = 12,
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
                                         tags$br()
                                       ),
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
                            ),
                            
                            
                            
                            tabPanel(strong("Resources"), 
                                     fluidPage(
                                       column(4,
                                              h3(strong("Google Earth Engine")),
                                              img(src = "GoogleEarthEngine.jpg", style = "display: inline; float: left;", width = "40%"),
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
                                              p("Zimbabwe National Statistics Agency is the statistics agency of Zimbabwe. We used ..."))
                                       
                                       )),
                            
                            
                            
                            
                 ),
                 
                 
                 ## Tab X Data-----------------------
                 tabPanel(strong("Data & Methodology"),
                          tabsetPanel(
                            tabPanel(strong("Data"),
                                     
                                     # tabName = "91_Dist",
                                     # # Everything has to be put in a row or column
                                     fluidRow(
                                       box(
                                         width = 6,
                                         withMathJax(),
                                         title = h1(strong("Remote Sensed Data")),
                                         h2(strong("Description of the Remote Sensed Data")),
                                         withMathJax(),
                                         h3(strong("EVI")),
                                         p("Description of EVI"),
                                         
                                         h3(strong("Precipitaion")),
                                         p("Description of Precipitation"),
                                         
                                         h3(strong("Soil Moisture")),
                                         p("Description of Soil Moisture"),
                                         
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
                                         width = 12,
                                         em(h4("A brief overview of the wrangling of the Remote Sensed data")), tags$br(),
                                       ),
                                       box(
                                         withMathJax(),
                                         title = h3(strong("MPI Methodology")),
                                         width = 12,
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
                                         tags$br()
                                       ),
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
                                         )),
                            
                            tabPanel(strong("Resources"), 
                                     fluidPage(
                                       column(4,
                                              h3(strong("Google Earth Engine")),
                                              img(src = "GoogleEarthEngine.jpg", style = "display: inline; float: left;", width = "40%"),
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
                                              p("Zimbabwe National Statistics Agency is the statistics agency of Zimbabwe. We used ..."))
                                       
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
                                    # tabName = "91_Dist",
                                     # # Everything has to be put in a row or column
                                     fluidRow(
                                       box(withSpinner(plotOutput("myplot3")),
                                         title = "Soil Moisture",
                                         width = 6,
                                         height = 600
                                       ),
                                         box(
                                           width = 4,
                                           withMathJax(),
                                           title = "Description",
                                           p("This graphic shows a detailed visualization of the Precipitation for the Zimbabwean districts and broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. There are three layers to this graph:"))))),
                            
                            
                            #),
                 
                 ## Tab 2
                 navbarMenu(strong("Multidimentional Poverty Index (MPI)"), 
                            tabPanel(strong("Multidimentional Poverty Index"),
                                     
                                     # tabName = "91_Dist",
                                     # # Everything has to be put in a row or column
                                     fluidRow(
                                       box(
                                         title = "Multidimentional Poverty Index",
                                         
                                         box(
                                           
                                           width = 4,
                                           withMathJax(),
                                           title = "Description",
                                           p("This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES, the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:"))))),
                            
                            tabPanel(strong("Components of the MPI"),
                                     
                                     # tabName = "91_Dist",
                                     # # Everything has to be put in a row or column
                                     fluidRow(
                                       box(
                                         title = "Components of the MPI",
                                         
                                         box(
                                           
                                           width = 4,
                                           withMathJax(),
                                           title = "Description",
                                           p("This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES, the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:"))))),
                            
                            
                            tabPanel(strong("Tab 3"),
                                     
                                     # tabName = "91_Dist",
                                     # # Everything has to be put in a row or column
                                     fluidRow(
                                       box(
                                         title = "Tab 3",
                                         
                                         box(
                                           
                                           width = 4,
                                           withMathJax(),
                                           title = "Description",
                                           p("This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES, the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:"))))),
                            
                            
                 ),
                 
                 
                 
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
                                   p("We would like to thank ",a(href="https://www.researchgate.net/profile/Tawanda-Chingozha","Tawanda Chingoza",target='_blank')," of Stellenbosch University for providing us with consultative insights on Zimbabwe and support on this project",". We also thank ZimStat for providing 2011 and 2017 PICES data for this project.")
                                   
                          )
                 ),
                 
                 
                 
                 ## References-------------------------------
                 tabPanel(strong("References"), value = "references",
                          column(3),
                          column(6, 
                                 h1(strong("References"), align = "center"),
                                 p("Milne, G., Mekonnen, A. F., & Benitez Ponce, P. C. (2019). Zimbabwe-Climate Smart Agriculture Investment Plan."),
                                 p("Reference 2"),
                                 p("Reference 3")
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


}

# Run the application ----------------
shinyApp(ui = ui, server = server)
