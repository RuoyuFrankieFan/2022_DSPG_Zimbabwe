#Leo's setting WD
# setwd("G:/My Drive/PhD/Internship/Zimbabwe/03_Git/2021_DSPG_Zimbabwe/ShinyApp")

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
#EVI_monthly <- read_csv("./Data/EVI_monthly.csv")
#AnnualEVI <- read_csv("./Data/EVI_annual.csv")
#zim_district <- st_read("./Shapefiles/Zim_D60.shp")  
#EVI_long <- read_csv("./Data/EVI_long.csv") 
#zim_region <- st_read("./Shapefiles/agro-ecological-regions.shp")


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
                 tabPanel("Overview", value = "overview",
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
                                          p("Prior research suggests that poverty in Zimbabwe has increased since the period of crisis began at the turn of the millennium. According to the latest World Bank (2020) estimates, due to the longstanding economic crisis and disruptions following the COVID-19 pandemic, 49% of Zimbabwe’s population was in extreme poverty in 2020. Our stakeholders seek solutions to the economic situation. They would like more granular information presented in creative ways that allow the user to glean the multidimensional and temporal aspects of poverty in Zimbabwe. The recent availability of household surveys for public use has opened the possibility of using the data to inform evidence-based policy."),
                                          p("This project uses data from the Poverty, Income, Consumption, Expenditure Survey (PICES) to provide granular information on poverty in Zimbabwe. We created multidimensional poverty indices (MPI) at the", strong(" district and province level"), " and decomposed them into components that focus on ", strong("education, health, employment, housing conditions, living conditions, assets, agricultural assets, and access to services."),   
                                            "We provide interactive tools that allow the user to visualize and study each component and understand their contribution to the MPI. We constructed these measures for two waves of data, 2011 and 2017, to show the changes in poverty over time and across regions.  The composition and decomposition of MPI in this project provide evidence-based policy recommendations and interventions for poverty reduction. ")),
                                   column(4,
                                          h2(strong("Introduction to Zimbabwe"), align = "center"),
                                          p("Nestled in the Southeastern tip of Africa, Zimbabwe neighbors South Africa, Mozambique, Zambia, and Botswana. Zimbabwe gained independence from Great Britain in 1980 and was ruled by Prime Minister and eventually President Robert Mugabe until his resignation in 2017. Presently, Emmerson Mnangagwa holds office. The country is home to roughly 14,830,000 inhabitants, 10% of whom live in the capital city of Harare. Although large agglomerations exist in other major urban areas, including Bulawayo and Chitungwiza, the population distribution is relatively evenly dispersed throughout the country otherwise. Zimbabwe’s central government is responsible for regulating its ten provinces, and 59 further subdivided districts. Zimbabwe’s terrain consists mainly of a flat plateau upon which forests thrive."), 
                                          p("Arable land is plenty, and  67.5 percent of the labor force works in agriculture growing sugar cane, tobacco, fruit, and vegetables, among other things. Another 7.3% of the labor force takes advantage of Zimbabwe’s rich natural resources and participates in mining. Zimbabwe exports coal, gold, platinum, copper, and other metals and manufacturing wood products, cement, chemicals, fertilizer, and food. Despite being relatively well-educated and highly literate, the population suffers from both unemployment and severe underemployment. Many individuals are either overqualified for the jobs they have or are engaging in full-time work. Together with low wages, this creates an obstacle to economic growth."),
                                          p("Monetary poverty measures in 2017 revealed that roughly 63% of Zimbabwean households lived in poverty. Together with the high poverty rate, the country experiences income inequality, malnourishment, low life expectancy, high infant/maternal mortality rates, difficulty accessing health and education resources, and overall low living standards.")),
                                   
                                   column(4,
                                          h2(strong("Agricultural Profile"), align = "center"),
                                          p("After gaining independence in 1980, there was widespread hope that the economic and labor exploitation Africans suffered at the hands of an imperial Great Britain would diminish. 
                                            While initial trends were encouraging, this hope dwindled as many factors sent the Zimbabwean economy into decline. Most prominent among these factors was the central government’s inconsistent policy, 
                                            which resulted in vague and evolving strategies for combatting poverty. A scientific socialist policy approach was applied between 1980 and 1990 to address poverty but was ineffective and abandoned due to a 
                                            financial downturn coupled with a prolonged drought which forced agricultural workers into the cities where they faced even greater poverty due to unemployment. In an attempt to revamp the economy, 
                                            Zimbabwe sought help from the International Monetary Fund (IMF) and the World Bank (WB), which led to adopting a different approach to economic development. The costs of necessities, including food, water, and education, 
                                            went up, harming and expanding the already existing poor population. The late 1990s and 2000s brought greater poverty and financial distress to Zimbabwe ever. 
                                            A continuing government budget deficit mixed with a fiscal policy focused on increasing the amount of money in circulation which resulted in hyperinflation. 
                                            In turn, this increased the economic crisis as foreign investment dropped and Zimbabwean currency crashed. During this time, unemployment skyrocketed, and a massive informal sector of the economy emerged. 
                                            In 2009, Zimbabwe adopted the US dollar along with a handful of other currencies. Though this move somewhat stabilized the economy initially, a 2013 shift in government rendered these efforts futile. 
                                            By 2017, inflation increased significantly, as did the overall economic crisis and poverty."))
                                   
                                   
                                   
                                   
                          ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: August 2022'))))
                 ),
                 
                 ## Tab data and methodology ----------------------------------------------------
                 navbarMenu("Data & Methodology", 
                            
                            tabPanel("Data", 
                                     fluidPage(
                                       h3(strong("Description of the PICES DATA")),
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
                                       h3(strong("Sensitivity Check")),
                                       p("Our stakeholders believe that given the country’s high level of literacy, a higher education threshold would more accurately represent education (or the lack thereof) in the Zimbabwean context. To understand how sensitive the MPI measures are to a change in definition, we construct an adjusted MPI. The adjusted MPI assumes that a household is deprived if no one in the household has attained a secondary school education. This is an expansion of the original definition in Alkire-Foster MPI, which sets the threshold at primary school."),
                                       p("We present this sensitivity analysis in the MPI Mapping tab. Select the education-adjusted MPI to compare with the original Alkire-Foster MPI results with the lower education threshold.")
                                     )
                                     
                                     
                                     
                                     
                            ),
                            tabPanel("Methodology", 
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
                            
                            
                            
                            tabPanel("Resources", 
                                     fluidPage(
                                       column(4,
                                              h3(strong("Google Earth Engine")),
                                              img(src = "GoogleEarthEngine.jpg", style = "display: inline; float: left;", width = "40%"),
                                              withMathJax(),  
                                              p("Google Earth Engine combines a multi-petabyte catalog of satellite imagery and geospatial datasets with planetary-scale analysis capabilities and makes it available for scientists, researchers, and developers to detect changes, map trends, and quantify differences on the Earth's surface. We used it to collect data on NDVI, EVI, precipitation and Soil moisture in Zimbabwe.")),
                                       
                                       column(4,
                                              h3(strong("Google Maps")),
                                              img(src = "data-gmaps.png", style = "display: inline; float: left;", width = "150px"),
                                              withMathJax(), 
                                              p("Google Maps is a comprehensive web mapping service created by Google. Its goal is to provide an interactive map of all the geographical contents of the world. This resource has a variety of uses, ranging from examining all service locations within a city to finding the quickest route between locations. It provides data at latitude and longitude level. We used Google Maps to visualize weather information behind the Google Earth Engine.")),
                                       
                                       column(4,
                                              h3(strong("ZimStat")),
                                              img(src = "zimstat_logo.png", style = "display: inline; float: left;", width = "150px"),
                                              withMathJax(), 
                                              p("Google Maps is a comprehensive web mapping service created by Google. Its goal is to provide an interactive map of all the geographical contents of the world. This resource has a variety of uses, ranging from examining all service locations within a city to finding the quickest route between locations. It provides data at latitude and longitude level. We used Google Maps to visualize weather information behind the Google Earth Engine.")),
                                       column(4,
                                              h3(strong("Other")),
                                              img(src = "zimstat.jpg", style = "display: inline; float: left;", width = "150px"),
                                              withMathJax(), 
                                              p("Google Maps is a comprehensive web mapping service created by Google. Its goal is to provide an interactive map of all the geographical contents of the world. This resource has a variety of uses, ranging from examining all service locations within a city to finding the quickest route between locations. It provides data at latitude and longitude level. We used Google Maps to visualize weather information behind the Google Earth Engine."))
                                       
                                       )),
                            
                            
                            
                            
                 ),
                 
                 
                 ## Tab X------------
                 tabPanel("Data & Methodology",
                          tabsetPanel(
                            tabPanel("Data",
                                     
                                     # tabName = "91_Dist",
                                     # # Everything has to be put in a row or column
                                     fluidRow(
                                       box(
                                         title = "Data",
                                         
                                         box(
                                           
                                           width = 8,
                                           withMathJax(),
                                           title = "PICES Data",
                                           p("This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES, the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:"))))),
                            
                            tabPanel("Methodology",
                                     
                                     # tabName = "91_Dist",
                                     # # Everything has to be put in a row or column
                                     fluidRow(
                                       box(
                                         title = "Methodology",
                                         
                                         box(
                                           
                                           width = 4,
                                           withMathJax(),
                                           title = "Description",
                                           p("This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES, the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:"))))),
                            
                            tabPanel("Resources",
                                     
                                     # tabName = "91_Dist",
                                     # # Everything has to be put in a row or column
                                     fluidRow(
                                       box(
                                         title = "Resources",
                                         
                                         box(
                                           
                                           width = 12,
                                           withMathJax(),
                                           title = "Description",
                                           p("This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES, the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:")))))
                          )),
                 
                 

                 ## Tab 1
                 navbarMenu("Remote Sensed Data", 
                            tabPanel("Enhanced Vegetation Index",
                                     
                                     # tabName = "91_Dist",
                                     # # Everything has to be put in a row or column
                                     fluidRow(
                                       box(
                                         title = "Enhanced Vegetation Index (EVI)",
                                         
                                         box(
                                           
                                           width = 4,
                                           withMathJax(),
                                           title = "Description",
                                           p("This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES, the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:"))))),
                            
                            tabPanel("Precipitation (Rainfall)",
                                     
                                     # tabName = "91_Dist",
                                     # # Everything has to be put in a row or column
                                     fluidRow(
                                       box(
                                         title = "Precipitation (Rainfall)",
                                         
                                         box(
                                           
                                           width = 4,
                                           withMathJax(),
                                           title = "Description",
                                           p("This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES, the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:"))))),
                            
                            
                            tabPanel("Soil Moisture",
                                    # tabName = "91_Dist",
                                     # # Everything has to be put in a row or column
                                     fluidRow(
                                       box(
                                         title = "Soil Moisture",
                                         
                                         box(
                                           
                                           width = 4,
                                           withMathJax(),
                                           title = "Description",
                                           p("This graphic shows a detailed visualization of Zimbabwean districts/provinces, broken up into distinct regions. In 2011 Zimbabwe was divided into 60 administrative districts. In 2017 PICES, the districts were redefined to include specific urban areas as separate districts, thus increasing the administrative boundaries to 91 districts. There are three layers to this graph:"))))),
                            
                            
                            ),
                 
                 ## Tab 2
                 navbarMenu("Multidimentional Poverty Index (MPI)", 
                            tabPanel("Multidimentional Poverty Index",
                                     
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
                            
                            tabPanel("Components of the MPI",
                                     
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
                            
                            
                            tabPanel("Tab 3",
                                     
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
                 navbarMenu("MPI and Indices", 
                            tabPanel("MPI & EVI",
                                     
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
                            
                            tabPanel("MPI & Precipitation",
                                     
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
                            
                            
                            tabPanel("MPI & Soil Moisture",
                                     
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
                 

                 tabPanel("Takeaways",
                          
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   column(3),
                                   column(6,
                                          h1(strong("Takeaways"),align="center"),
                                          p("The analysis presented here provides an interactive way to decompose a multidimensional poverty index along many dimensions. We display the MPIs in maps and rankings, allowing users to assess multidimensional poverty by region (10 provinces/60 districts/91 districts), by population (all/urban/rural households), and by their preferred definition of poverty (k-threshold). We allow users to decompose the MPIs into their 14 components, allowing users to look at the unidimensional poverty indices of the individual components and their contribution to the multidimensional indices. Finally, we offer users the ability to view the change in multidimensional poverty between the two most recent waves of PICES surveys (2011-2017). "),
                                          p("From our analyses, we see that when defining multidimensional poverty according to low thresholds, major urban areas such as Bulawayo and Harare are, on average, better off than their rural counterparts. Despite this, as we increase the threshold of multidimensional poverty, we see that these urban areas host the most vulnerable populations. We notice that specific components contribute more extensively to the national indices (i.e., Chronic Illness, Lack of Access to Services, Lack of Household Assets), whereas others contribute more to urban households (Lack of Health Visit) or rural households (Poor Cooking Fuel). Our sensitivity check shows that by setting a higher poverty line for a key component like Max Education, thereby considering more households deprived in that dimension, multidimensional poverty increases accordingly. Finally, the multidimensional poverty indices constructed here show that poverty has increased across most districts in Zimbabwe from 2011 to 2017. "),
                                          p("We note that trends within our analyses are not unanimous. Individual districts do not necessarily follow national trends. The interactive dashboard presented here, providing users the ability to disaggregate the analysis from the province to the district level, allows for a more refined assessment of the trends in the individual districts."),
                                          p(""),
                                   )
                                   
                          )
                          
                 ),
                 
                 
                 
                 
                 ## Tab DSPG Team------------------------------------------------
                 tabPanel("Our Team", 
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
                                          p(a(href = 'https://www.linkedin.com/in/atticus-rex-717581191/', 'Frankie Fan', target = '_blank'), "(Virginia Tech, Computational Modeling and Data Analytics);"),
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
                                   p(a(href="https://www.linkedin.com/in/dhiraj-sharma-aa029024/?originalSubdomain=np","Dhiraj Sharma",target='_blank')," (World Bank); "),
                                   # p("TAWANDA CHINGOZHA (STELLENBOSCH UNIVERSITY) ",a(href="https://www.zimstat.co.zw/","(Zimbabwe National Statistics Agency)",target="_blank")),
                                   
                                   p(em("Disclaimer: "),("This project is an academic exercise conducted by VT-Data Science for the Public Good. The findings, interpretations, and conclusions expressed here do not necessarily reflect the views of the World Bank or the Zimbabwe Statistical Agency."))
                                   
                          ),
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   h4(strong("Acknowledgement"), align = "center"),
                                   p("We would like to thank ",a(href="https://www.researchgate.net/profile/Tawanda-Chingozha","Tawanda Chingoza",target='_blank')," of Stellenbosch University for providing us with consultative insights on Zimbabwe and support on this project",". We also thank ZimStat for providing 2011 and 2017 PICES data for this project.")
                                   
                          )
                 ),
                 
                 
                 
                 ## References-------------------------------
                 tabPanel("References", value = "references",
                          column(3),
                          column(6, 
                                 h1(strong("References"), align = "center"),
                                 p("Acharya, T., Lee, D., Yang, I., & Lee, J. (2016). Identification of water bodies in a Landsat 8 Oli image using A j48 decision tree. Sensors, 16(7), 1075. https://doi.org/10.3390/s1607107"),
                                 p("Akhtar Ali Memon, Sher Muhammad, Said Rahman, Mateeul Haq, Flood monitoring and damage assessment using water indices: A case study of Pakistan flood-2012, The Egyptian Journal of Remote Sensing and Space Science, Volume 18, Issue 1, 2015, Pages 99-106, ISSN 1110-9823. "),
                                 p("Bakar, S.B.A. â Pradhan, B. â Lay, U.S., Abdullahi, S. (2016): Spatial assessment of land surface temperature and land use/land cover in Langkawi Island. 8th IGRSM International Conference and Exhibition on Remote Sensing & GIS (IGRSM 2016) IOP Publishing IOP Conf. Series: Earth and Environmental Science 37 (2016) 012064 doi:10.1088/1755- 1315/37/1/012064"),
                                 p("Bao, Z., Zhang, J., Wang, G., Guan, T., Jin, J., Liu, Y., Li, M., & Ma, T. (2021). The sensitivity of vegetation cover to climate change in multiple climatic zones using machine learning algorithms. Ecological Indicators, 124, 107443. https://doi.org/10.1016/j.ecolind.2021.107443 "),
                                 p("Chakroun, H.; Mouillot, F.; Hamdi, A. Regional equivalent water thickness modeling from remote sensing across a tree cover/LAI gradient in Mediterranean forests of Northern Tunisia. Remote Sens. 2015, 7, 1937â1961. "),
                                 p("Chen, X.-L. â Zhao, H.-M. â Li, P.-X. â Yin, Z.-Y. (2006): Remote sensing image-based analysis of the relationship between urban heat island and land use/cover changes. Remote Sensing of Environment. 104: 133-146"),
                                 p("Conversion to TOA Radiance. Using the USGS Landsat Level-1 Data Product. (n.d.). https://www.usgs.gov/core-science-systems/nli/landsat/using-usgs-landsat-level-1-data-product."),  
                                 p("Dawson, T., Sandoval, J. S., Sagan, V., & Crawford, T. (2018). A spatial analysis of the relationship between vegetation and poverty. ISPRS International Journal of Geo-Information, 7(3), 83. https://doi.org/10.3390/ijgi7030083"),
                                 p("El Bastawesy, M., Gabr, S., Mohamed, Ihab, 2015. Assessment of hydrological changes in the Nile River due to the construction of Renaissance Dam in Ethiopia. Egypt. J. Remote Sens. Space Sci. 18 (1), 65â75."),
                                 p("Fensholt, R.; Rasmussen, K.; Nielsen, T.T.; Mbow, C. Evaluation of earth observation based long term vegetation trendsâNtercomparing NDVI time series trend analysis consistency of Sahel from AVHRR GIMMS, Terra MODIS and SPOT VGT data. Remote Sens. Environ. 2009, 113, 1886â1898"),
                                 p("Gao, B.-C. NDWIâA normalized difference water index for remote sensing of vegetation liquid water from space. Remote Sens. Environ. 1996, 58, 257â266."),
                                 p("Gao, B.-cai. (1996). NDWIâA normalized Difference WATER index for remote sensing of VEGETATION liquid water from space. Remote Sensing of Environment, 58(3), 257â266. https://doi.org/10.1016/s0034-4257(96)00067-3"), 
                                 p("Gitelson, A.A.; Peng, Y.; Huemmrich, K.F. Relationship between fraction of radiation absorbed by photosynthesizing maize and soybean canopies and NDVI from remotely sensed data taken at close range and from MODIS 250m resolution data. Remote Sens. Environ. 2014, 147, 108â120."),
                                 p("Gu, Y. â Hunt, E. â Wardlow, B. â Basara, J.B. â Brown, J.F. - Verdin, J.P. (2008): Evaluation of MODIS NDVI and NDWI for vegetation drought monitoring using Oklahoma Mesonet soil moisture data, Geophysical Research Letters 35: L22401, doi:10.1029/2008GL035772."),
                                 p("Herrero, H., Waylen, P., Southworth, J., Khatami, R., Yang, D., & Child, B. (2020). A healthy Park NEEDS HEALTHY Vegetation: The story OF Gorongosa National Park in the 21st century. Remote Sensing, 12(3), 476. https://doi.org/10.3390/rs12030476"),  
                                 p("I.H.El-Gamily,G.Selim,E.A.Hermas, Wireless mobile field-based GIS science and technology for crisis management process: a case study of a fire event, Cairo, Egypt, Egypt. J. Remote Sens. Space Sci.,13(1)(2010), pp.21-29"), 
                                 p("Jackson, T.J.; Chen, D.; Cosh, M.; Li, F.; Anderson, M.; Walthall, C.; Doriaswamy, P.; Hunt, E.R. Vegetation water content mapping using Landsat data derived normalized difference water index for corn and soybeans. Remote Sens. Environ. 2004, 92, 475â482. "),
                                 p("Karnieli, A.; Agam, N.; Pinker, R.T.; Anderson, M.; Imhoff, M.L.; Gutman, G.G.; Panov, N.; Goldberg, A. Use of NDVI and land surface temperature for drought assessment: Merits and limitations. J. Clim. 2010, 23, 618â633."),
                                 p("Landsat surface REFLECTANCE-DERIVED SPECTRAL Indices. Landsat Normalized Difference Vegetation Index. (n.d.). https://www.usgs.gov/core-science-systems/nli/landsat/landsat-normalized-difference-vegetation-index?qt-science_support_page_related_con=0#qt-science_support_page_related_con. "),  
                                 p("Liu, W. â Lu, L. â Ye, C. â Liu, Y. (2009.): Relating urban surface temperature to surface characteristics in Beijing area of China. Proc. SPIE 7498, MIPPR 2009: Remote Sensing and GIS Data Processing and Other Applications, 74982I (30 October 2009); doi: 10.1117/12.833679"), 
                                 p("Ogashawara, I. â Bastos, V.S.B. (2012): A Quantitative Approach for Analyzing the Relationship between Urban Heat Islands and Land Cover. Remote Sensing. 4: 3596-3618. "), 
                                 p("Ouzemou, J.-E., El Harti, A., Lhissou, R., El Moujahid, A., Bouch, N., El Ouazzani, R., Bachaoui, E. M., & El Ghmari, A. (2018). Crop type mapping FROM pansharpened Landsat 8 NDVI data: A case of a highly fragmented and intensive agricultural system. Remote Sensing Applications: Society and Environment, 11, 94â103. https://doi.org/10.1016/j.rsase.2018.05.002"),
                                 p("Pettorelli, N. (2013). The normalized difference vegetation index. Oxford University Press. "),  
                                 p("Piragnolo, M.; Pirotti, F.; Guarnieri, A.; Vettore, A.; Salogni, G. Geo-spatial support for assessment of anthropic impact on biodiversity. Int. J. Geo-Inf. 2014, 3, 599â618. "),
                                 p("Pravalie, R., SÃ®rodoev, I., & Peptenatu, D. (2014). Detecting climate change effects on forest ecosystems in southwestern Romania USING Landsat TM Ndvi data. Journal of Geographical Sciences, 24(5), 815â832. https://doi.org/10.1007/s11442-014-1122-2"), 
                                 p("S.Lu,B.Wu,N.Yan,H.Wang, Water body mapping method with HJ-1A/B satellite imagery, Int. J. Appl. Earth Obs. Geoinf.,13(3)(2011), pp.428-434"), 
                                 p("S.K.McFeeters, The use of the normalized difference water index (NDWI) in the delineation of open water features, Int. J. Remote Sens.,17(7)(1996), pp.1425-1432"), 
                                 p("SÃ¡nchez-Ruiz, S.; Piles, M.; SÃ¡nchez, N.; MartÃ­nez-FernÃ¡ndez, J.; Vall-llossera, M.; Camps, A. Combining SMOS with visible and near/shortwave/thermal infrared satellite data for high resolution soil moisture estimates. J. Hidrol. 2014, 516, 273â283."),
                                 p("Serrano, J; Shahidian, S.; Marques da Silva, J. (2019) Evaluation of Normalized Difference Water Index as a Tool for Monitoring Pasture Seasonal and Inter-Annual Variability in a Mediterranean Agro-Silvo-Pastoral System. Water, 11, 62; doi:10.3390/w11010062"), 
                                 p("Su, H., Yang, D., & Yong, Y. (2015). MODIS-Landsat data fusion for Estimating Vegetation dynamics - a case study for Two ranches in SOUTHWESTERN TEXAS. Proceedings of 1st International Electronic Conference on Remote Sensing. https://doi.org/10.3390/ecrs-1-d016"), 
                                 p("Wang, X.; Fuller, D.O.; Setemberg, L.; Miralles-Wilhelm, F. Foliar nutrient and water content in subtropical tree islands: A new chemohydrodynamic link between satellite vegetation indices and foliar Î´ 15N values. Remote Sens. Environ. 2011, 3, 923â930."),
                                 p("Zhu, Y., Yang, K., Pan, E., Yin, X., & Zhao, J. (2018). Extraction and analysis of urban vegetation information based on remote sensing image. 2018 26th International Conference on Geoinformatics. https://doi.org/10.1109/geoinformatics.2018.8557075"))
                 ),
                 inverse = T)



## Define server logic required
server <- function(input, output) {
  # Run JavaScript Code
  runjs(jscode)
  
#    output$distPlot <- renderPlot({
#        # generate bins based on input$bins from ui.R
#        x    <- faithful[, 2]
#        bins <- seq(min(x), max(x), length.out = input$bins + 1)
#
#        # draw the histogram with the specified number of bins
#        hist(x, breaks = bins, col = 'darkgray', border = 'white')
#    })
}

# Run the application 
shinyApp(ui = ui, server = server)
