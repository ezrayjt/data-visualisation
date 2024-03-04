library(shiny)
library(leaflet)
library(tmaptools)
library(ggplot2)

frogs = read.csv("PE2_frog_data.csv")

ui <- fluidPage(
  
  titlePanel("Frogs observed in ourter eastern melbourne, 2000-2018"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs",
                  "number of observations:",
                  min = 1,
                  max = 921,
                  value = 300),
      selectInput("Genus", "Choose Genus:", 
                         choices = as.list(frogs[!duplicated(frogs$Genus), "Genus"]))
    ),
    
    mainPanel(
      leafletOutput("frogMap"),
      plotOutput("frogChart")
    )
  )
)

server <- function(input, output) {
  
  output$frogChart = renderPlot({
    ggplot(frogs, aes(Genus, Habitat))+geom_point()+
      labs(title = " frog genera and terrain")})
  
  output$frogMap = renderLeaflet({
    
    frogs$Latitude = round(frogs$Latitude, 3)
    frogs$Longitude = round(frogs$Longitude, 3)
    
    # have random observations from frogs data as the amount input
    frogs = frogs[sample(nrow(frogs), input$obs), ]
    # take the input genus and filter the data
    frogs = subset(frogs, Genus == input$Genus)
    
    # get the coordinate of melbourne
    melbourne = as.vector(geocode_OSM("Melbourne")$coords)
    
    leaflet(data = frogs) %>%
      addTiles() %>%
      addMarkers(~Longitude, ~Latitude, popup = ~as.character(Genus), clusterOptions = markerClusterOptions()) %>%
      setView(lng = melbourne[1], lat=melbourne[2] , zoom = 10)
  })
}


shinyApp(ui = ui, server = server)




