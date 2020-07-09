#Packages
library(shiny)
library(ggplot2)
library(tidyverse)
install.packages("mapdata")
library(mapdata)

#set working directory
#setwd("~/Desktop/bee_map_shiny_app")

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
                  choices = list("Andrenidae", "Apidae", "Colletidae", "Halictidae", "Megachilidae", "Melittidae"),
                  selected = FALSE,
                  multiple = FALSE,
                  selectize = FALSE, size = 4),
      selectInput(inputId = "grade", 
                  label = "Select grade:",
                  choices = list("research", "casual", "needs_id"))
    ),
    
    # Mainpanel with ggplot output
    mainPanel(plotOutput("heatMap"))
  )
)

#Server.R
server <- function(input, output) {
  
  #reactive data for filtering out by family
  dat <- reactive(
    bees_us %>% 
      filter(taxon_family_name == input$family) %>% 
      filter(quality_grade == input$grade)
  )
  
  #using filtered data to make map
  output$heatMap <- renderPlot({
    ggplot(dat()) +
      geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill="lightgray", colour = "white")+
      geom_bin2d(aes(x = longitude, y = latitude), bins = 100)+
      scale_fill_steps2(low = "#91bfdb", mid = "#ffffbf", high = "#fc8d59") +
      xlim(-125,-65)+
      ylim(20,50)+
      theme_minimal() 
  }, 
  height = 375,  
  width = 600)
}

# Run the application 
shinyApp(ui = ui, server = server)
