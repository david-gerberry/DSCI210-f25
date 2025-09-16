library(shiny)
library(tidyverse)
library(shinycssloaders)
library(leaflet)


test = function(map) {
  if (map == "CPS") {
  hist(starwars$mass, breaks = 100)
  }
else if (map == "MUN") {
  hist(starwars$birth_year)
}
else {
  hist(starwars$height)
}
  
}


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
      h3("Precinct Map"),
      withSpinner(
        leafletOutput("map_result", height = 400), 
        type = 8, color = '#7B4BCC', proxy.height = 196
      )
    )
  ),#end column
  column( width = 6,
    div(
      class = "Graph Display",
      h3("Test"),
      withSpinner(
        plotOutput("test1"),
        type = 8, color = '#7B4BCC', proxy.height = 196
      ),
      withSpinner(
        textOutput("test2"),
        type = 8, color = '#7B4BCC', proxy.height = 196
      )
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
  output$test1 <- renderPlot({
    test(input$map_dropdown)
  })
  output$test2 <- renderPrint({
    click <- input$map_result_shape_click
    if (is.null(click)) {
      "Click on a shape"
    } else {
      paste("You clicked:", click$id)
    }
  })
}

shinyApp(ui, server)
