library(shiny)
library(shinycssloaders)
library(leaflet)

ui <- fluidPage(
  tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Open+Sans&display=swap"),
  
  tags$style(HTML("
    body, label, input, button, select, h1, h2, h3, h4, h5, h6, .shiny-text-output {
      font-family: 'Open Sans', sans-serif;
    }
    .shiny-text-output, .shiny-html-output {
      font-size: 20px;
    }
    
    .grid-container {
      display: grid;
      grid-template-columns: 1fr 2fr;
      grid-template-areas:
        'sidebar map'
        'cost table'
        'cost table'
        'cost table';
      gap: 20px;
      padding: 10px;
    }
    input[type='checkbox'] {
      accent-color: #7B4BCC;
    }
    input[type='checkbox'] + label {
      color: #7B4BCC;
    }
    .bottom-container {
      display: grid;
      grid-template-columns: 1fr 1fr 1fr;
      grid-template-areas: 'cost table average';
      gap: 20px;
    }
    .sidebar  { grid-area: sidebar; }
    .map      { grid-area: map; }
    .cost     { grid-area: cost; }
    .table    { grid-area: table; }
    .average  { grid-area: average; }
    .irs-bar {
      background: #7B4BCC !important;
      border-top: 1px solid #7B4BCC !important;
      border-bottom: 1px solid #7B4BCC !important;
    }
    .irs-line {
      background: #e0e0e0;
      border: 1px solid #ccc;
    }
    .irs-slider {
      background: #7B4BCC !important;
      border: 1px solid #5F3C9F !important;
    }
    .irs-single,
    .irs-from,
    .irs-to {
      background-color: #7B4BCC !important;
      color: white !important;
      border: none !important;
    }
    table.model-table {
      width: 100%;
      border-collapse: collapse;
    }
    table.model-table th, table.model-table td {
      border: 1px solid #ddd;
      padding: 8px;
      text-align: center;
    }
    table.model-table th {
      background-color: #f2f2f2;
    }
  ")),
  
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
