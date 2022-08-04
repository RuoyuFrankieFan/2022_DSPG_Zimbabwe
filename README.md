# Using Remote Sensed Data for Social &amp; Economic Decision Making in Zimbabwe

## About the repo:
This is the 2022 DSPG Zimbabwe Project #2022_DSPG_Zimbabwe. This code repository contains R-scripts, shapefiles, images, and data that are used as a framework for building our ShinyApp project.
We use the Google Earth Engine using the Python API to get weather data. These codes are currently in use for the 'Data Science for Public Good' program taught at Virginia Tech in the summer of 2022 and are available for use. Users are encouraged to work with their local environment, but the Google Earth Engine data are run on Google Colab (other prefered python based platform).
# 

# Files
## Codes
To view the code we used to process our weather indices and other data please visit:
### Review Folder - Data Processing

In this folder, you could find files used for data processing.
- "EVI processing.Rmd" was used to aggregate daily EVI to monthly and annually data, and "EVI-further processing.Rmd" further processed the data by growing seasons in 2010-2011 and 2016-2017.
- "Template for Data Processing.Rmd" provides guideline to reshape the csv files of remote-sensed data downloaded from Google Earth Engine.

- "rain.Rmd" was used to process the precipitation data by growing seasons in 2010-2011 and 2016-2017.

- "soilsurf.Rmd" was used to process the surface soil moisture data by growing seasons in 2016-2017.

- "soilperc.Rmd" was used to process the precent soil moisture data by growing seasons in 2016-2017.
##

## Data
### Google Earth Data
To view the code we used to download our weather indices please visit: [link](https://colab.research.google.com/drive/163e-gYd_1DbGElMiTHTJrTtlnetVu11T?usp=sharing#scrollTo=MzNgO88VTWWQ).
The downloaded and processed data can be located in our data folder within our ShinyApp folder.


### PICES Data
We use the data from the [2021 Zimbabwe project](https://dspgtools.shinyapps.io/dspg21zimbabwe/). Here is the link to their repo: [link](https://github.com/yangcheng258/2021_DSPG_Zimbabwe). The specific data we use can also be located in our data within our ShinyApp folder.


## Images
The images we used and generated are in the www folder within our ShinyApp folder.


## Shiny App
To view the code we used to render our our Shiny App please visit the app.R file.




# 
For further questions, please reach out to

# Project Team

## Graduate Fellows:
- [Leonard-Allen Quaye](https://sites.google.com/vt.edu/leo-allen-quaye/home?authuser=0) (Virginia Tech, Agricultural and Applied Economics);
- [Poonam Tajanpure](https://www.bse.vt.edu/people/grad-students/poonam-tajanpure.html) (Virginia Tech, Biological Systems Engineering).

## Undergraduate Interns:
- [Frankie Fan](https://www.linkedin.com/in/frankie-ruoyu-fan/?lipi=urn%3Ali%3Apage%3Ad_flagship3_people_connections%3BBiz9W9pbRcO00B0bou%2F2vg%3D%3D) (Smith College & Brown University, Math and Data Science);
- [Ari Liverpool](https://www.linkedin.com/in/ari-l-12b151123/?lipi=urn%3Ali%3Apage%3Ad_flagship3_people_connections%3B5WMwWerMTvefiu%2Fq85Z5mw%3D%3D) (Virginia Tech, Applied Economics Management);
- [Josue Navarrete](https://www.linkedin.com/in/josue-navarrete-36a6321b4/?lipi=urn%3Ali%3Apage%3Ad_flagship3_people_connections%3B5WMwWerMTvefiu%2Fq85Z5mw%3D%3D) (MiraCosta College, Computer Science).

## Faculty & Associate Team Members:
- [Dr. Brianna Posadas](https://www.linkedin.com/in/briannaposadas/) (Virginia Tech, School of Plant and Environmental Sciences);
- [Dr. Susan Chen](https://aaec.vt.edu/people/faculty/chen-susan.html) (Virginia Tech, Agricultural and Applied Economics);
- [Dr. Jeffrey Alwang](https://aaec.vt.edu/people/faculty/alwang-jeffrey.html) (Virginia Tech, Agricultural and Applied Economics);
- [Naveen Abedin](https://www.linkedin.com/in/naveen-abedin-0ab1089a/?lipi=urn%3Ali%3Apage%3Ad_flagship3_people_connections%3BgdZR16ktRcatg1cpCMufuQ%3D%3D) (Virginia Tech, Agricultural and Applied Economics).
