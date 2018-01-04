library(shiny)
library(leaflet)
library(htmltools)
library(htmlwidgets)

dat <- read.csv('../data/country_kz.csv')
sum <- data.frame(
  country = dat$country,
  date = dat$date
)
  
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

