#Packages
library(shiny)
library(tidyverse)
library(ggplot2)

#set working directory
setwd("/Users/User_2/Desktop/bees")

#loading data
bees_raw <- read.csv("bees_raw.csv")

#filter to us records
bees_us <- bees_raw %>% 
  filter(place_country_name == "United States")

#map data for polygons
usa <- map_data("state")

#ui.R
ui <- fluidPage(
  
  # Title
  titlePanel("iNaturalist Bee Observations"),
  
  # Sidebar with drop-down for family names
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "family", 
                  label = "Select Family:",
                  choices = list("Andrenidae" = "Andrenidae", "Apidae" = "Apidae", "Colletidae" = "Colletidae", "Halictidae" = "Halictidae", "Megachilidae" = "Megachilidae", "Melittidae" = "Melittidae"))
    ),
    
    # Mainpanel with ggplot output
    mainPanel(
      plotOutput("heatMap")
    )
  )
)

#Server.R
server <- function(input, output) {
  
  #reactive data for filtering out by family
  dat <- reactive(
    bees_us %>% 
      filter(taxon_family_name == input$family))
  
  #using filtered data to make map
  output$heatMap <- renderPlot({
    ggplot(dat()) +
      geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill="lightgray", colour = "white")+
      geom_bin2d(aes(x = longitude, y = latitude), bins = 100)+
      scale_fill_continuous(type = "viridis") +
      xlim(-125,-65)+
      ylim(20,50)+
      theme_minimal() 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

