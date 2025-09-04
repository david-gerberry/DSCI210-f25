library(shiny)
library(shinycssloaders)
library(leaflet)

ui <- fluidPage(
  tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Open+Sans&display=swap"),
  
  titlePanel("Funky Census Map"),
  
  div(class = "map",
      h3("Service Area Map"),
      withSpinner(leafletOutput("map_result", height = 400), 
                  type = 8, color = '#7B4BCC', proxy.height = 196)
  )
)

server <- function(input, output, session) {
  output$map_result <- renderLeaflet({ mx4_parent() })
}

shinyApp(ui, server)
