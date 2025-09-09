library(shiny)
library(tidyverse)
library(shinycssloaders)
library(leaflet)

ui <- fluidPage(
  
  titlePanel("Funky Census Map"),
  
  # row for dropdowns
  fluidRow(
    column(
      width = 3,
      selectInput(inputId = "map_dropdown", 
                  label = "Choose a Campaign", 
                  choices = c("School Board" = "CPS",
                              "City Council" = "CIT",
                              "Judge" = "MUN"),
                  selected = "CPS")   # must match stored value
    ),
    column(
      width = 3,
      selectInput(inputId = "data_dropdown", 
                  label = "Choose a Demographic", 
                  choices = c("Funkyness" = "funk",
                              "Tomfunkery" = "tom",
                              "19-34 on Medicaid" = "medicaid"),
                  selected = "funk")  # must match stored value
    )
  ),
  
  # map output with spinner
  fluidRow(
    column( width = 6,
      div(
      class = "map",
      h3("Map"),
      withSpinner(
        leafletOutput("map_result", height = 400), 
        type = 8, color = '#7B4BCC', proxy.height = 196
      )
    )
  ),#end column
  column(
    div(
      class = ""
    )
  )
  )
)

server <- function(input, output, session) {
  output$map_result <- renderLeaflet({
    # Example placeholder (replace with your function)
    # Use the selected campaign
    shiny.map(input$map_dropdown)
  })
}

shinyApp(ui, server)
