---
title: "Post02 - Geocode, Leaflet and Map Visualization"
author: <font size="3">Katherine Zhou</font>
date: <font size="3">November 29, 2017</font>
output: html_document
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

<font size="6">Introduction</font>

<font size="4">In this post, I will introduce a function geocode() and a new package leaflet, and combine them with Shiny app to develop a simple app: "Hello World". Inspired by work we did in last lab and homework, I want to investigate a new method of map visualization to create a more user-friendly and interactive environment.</font>

<font size="6">Motivation</font>

<font size="4">Personally, I love travel. My goal is to travel all around the world. It would be great if there is an app that keeps track of all the places I have traveled and marks them on a world map. As we did in lab, there are several ways to achieve it, such as basic plot() or ggmap(). However, it would be tedious to find the longtitude and latitude of each country manually and the map is not dynamic enough to view. Thus, I want to introduce a new function geocode and a new package leaflet.

First of all, make sure you download this package and install it to your RStudio. Library it to get ready!</font>

```{r}
# install.packages("leaflet")
library(leaflet)
library(ggplot2)
library(ggmap) 
# geocode() is a function from ggmap()
```

<font size="6">Data Preparation with Geocode</font>

<font size="4">Instead of downloading data online, I will prepare a small dataset myself including all the countries I have been to and the dates when I visited them.</font>

```{r}
# data cleaning
visit <- c('china',
           'hong kong',
           'taiwan',
           'japan',
           'singapore',
           'malaysia',
           'sewden',
           'norway',
           'denmark',
           'united kingdom',
           'germany',
           'austria',
           'czech republic',
           'australia',
           'new zealand',
           'united states',
           'mexico'
           )
date <- c(1993,
          2011,
          2013,
          2006,
          2013,
          2013,
          2007,
          2007,
          2007,
          2008,
          2015,
          2013,
          2013,
          2012,
          2012,
          2012,
          2016)
# you can try to create your own data set
```

<font size="5">Geocode:</font>

<font size="4">This function takes names of places and returns their longitudes and latitudes. Here, visit_lon and visit_lat are two numeric vectors that contain the corresponding longitudes and latitudes.</font>

```{r message=FALSE}
visit_ll <- geocode(visit)
```
```{r}
visit_lon <- visit_ll$lon
visit_lat <- visit_ll$lat

dat <- data.frame(
  country=visit,
  lon=visit_lon,
  lat=visit_lat,
  date=date
)
print.data.frame(dat,right=F) 

# export data to a CSV file for storage and future reference
write.csv(dat,'data/country_kz.csv')
```

<font size="6">Leaflet</font>

<font size="5">Basic Usage</font>

```{r}
leaflet() %>% 
  addTiles() %>% 
# the easiest way to add tiles is by default when the OpenStreetMap is used
# you could also use other maps from online resources
  addMarkers(dat$lon, dat$lat,
            popup='',label= dat$country)
# the first input is longitude and the second is latitude
# label can be displayed on mouse over
# here, the label is country name
```

<font size="4">It's great that you could zoom in and out using "+" and "-" without extra coding. It's a built-in feature of leaflet.</font>

<font size="5">Icon Makers</font>

<font size="4">Leaflet is a highly customizable package. Here, I will show you how to change the icon shape. As a default, the icon marker is a blue dropped pin, as the one shown above. We can simply use makeIcon and choose other icons from online resources.</font>

```{r}
# creating an icon using makeIcon function. 
greenLeafIcon <- makeIcon(
  iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png", 
# shape of icon
  iconWidth = 38, iconHeight = 95,  
# these parameters define the dimensions of icon displayed 
  iconAnchorX = 22, iconAnchorY = 94, 
# point of the icon which corresponds to marker's location
  shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png", 
# shape of shadow 
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62 
# these parameters define the dimensions of shadow displayed 
)

# using greenLeafIcon as marker
leaflet() %>% addTiles() %>%
  addMarkers(dat$lon, dat$lat, icon = greenLeafIcon)
```


<font size="5">EasyButton and Shiny Integration</font>

<font size="4">EasyButton() and addEasyButton(), two functions in package leaflet, can be used to add buttons to the map, in addition to the existing buttons, zoom in and out.

Leaflet can be incorporated into Shiny using a leafletOutput() and assigning a renderLeaflet call to the output. It is the same idea as we did in the homework, just different functions.</font>

```{r results='hide'}
library(shiny)
# shiny app
ui <- fluidPage(
  titlePanel("Hello World"),
  sidebarLayout(
    
    sidebarPanel(
    h3('Summary'),
    verbatimTextOutput('sum')
    ),
    
    mainPanel(
    leafletOutput('mymap'),
    print('Total'),
    verbatimTextOutput('total')
    )
)
)

server <- function(input, output) {

  output$mymap <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      addMarkers(dat$lon, dat$lat,
                 popup='',label= dat$country)%>%
# easyButton can create customized buttons and addEasyButton adds them to the map
# the first button returns zoom level to the default and the second button marks your current location
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
      addEasyButton(easyButton(
        icon='fa-crosshairs', title='Locate Me',
        onClick=JS("function(btn, map){ map.locate({setView: true});}")))
    })
  
  output$sum <- renderPrint({
  print.data.frame(sum,row.names = F, right = F)
   })
  
  output$total <- renderText({
    length(dat$country)
   })
  
}

shinyApp(ui = ui, server = server)
```

<font size="4">Screenshoot of this Shiny app</font>

```{r out.width = '150%'}
library(knitr)
include_graphics('images/helloworld.png',auto_pdf = TRUE)
# this figure gives us a good demonstration on how it works
```
<font size="6">Discussion and Conclusion</font>

<font size="4">As what I have demonstrated above, function geocode() and package leaflet offer users a new way to develop maps and they can even be incorporated into Shiny app for a more dynamic environment. The app, "Hello World", is just a simple example of how to utilize them to meet user's need. 

I hope that RStudio is not just a subject learnt and being tested in Stat 133. It can be extended to our daily life as an interesting and useful tool.</font>

<font size="6">Reference</font>

<font size="4">Leaflet (https://rstudio.github.io/leaflet/)<br />
Geocode (https://www.rdocumentation.org/packages/ggmap/versions/2.6.1/topics/geocode)<br />
Basemaps (https://rstudio.github.io/leaflet/basemaps.html)<br />
Icon Markers (https://rstudio.github.io/leaflet/markers.html)<br />
Shiny Integration (https://rstudio.github.io/leaflet/shiny.html)<br />
Additional Features (https://rstudio.github.io/leaflet/morefeatures.html)<br />
easyButtonState (https://www.rdocumentation.org/packages/leaflet/versions/1.1.0/topics/easyButtonState)<br /></font>
