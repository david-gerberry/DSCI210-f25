library(shiny)
library(tidyverse)
library(shinycssloaders)
library(leaflet)
library(sf)
library(tidycensus)
load("data/acs_data.RData")

#' Creates a map
#' @param shpFile what map we're going to use ( all caps! )
#'    "CPS" = cincinatti public school
#'    "MUN" = municipal court 4
#'    "CIT" = cincinatti city council
#'    
#' @param colType what columns we are going to look at ( all lowercase! )
#'    "age" =
#'    "income" =
#'    "race" =
#' @note make sure you load acs_data.RData in DSCI210-f25/data
#' 
shiny.map <- function(shpFile = "CIT",colType = "age"){
  
  # LETS GET OUR DATA READY
  mapDict = c(
    "CPS" = "acs_interp_cps", #cincinatti public school
    "MUN" = "acs_interp_judicial", #municipal court
    "CIT" = "acs_interp_cincy" #city council!
  )
  columnId = c(
    "age" = "median_ageE", #cincinatti public school
    "income" = "med_incomeE", #municipal court
    "race" = "pop_totalE" #city council!
  )
  
  colName = columnId[colType] # get our actual column
  
  ourMap = get( mapDict[shpFile] ) %>%  # get our dataset and set it up for maps
    st_zm() %>% 
    st_as_sf(4269)
  
  
  # PREPARE VISUALS FOR THE GRAPH
  palette <- colorNumeric(
    palette = "plasma", # purple to yellow color scale
    domain = ourMap[[colName]]
  )
  
  ourClr = ~palette( ourMap[[colName]] ) # get our colors
  
  
  mapFullName = c( # get the full name of the place we're looking for
    "CPS" = "Cincinatti Public Schools",
    "MUN" = "Municipal Court District 4", 
    "CIT" = "City Council" 
  )[shpFile]
  
  columnFullName = c( # get the full name for the variable we're showing
    "age" = "Median Age", 
    "income" = "Median Income", 
    "race" = "Total Population"
  )[colType]
  
  # for specifying unit type
  displayPrefix = c(
    "age" = "",
    "income" = "$",
    "race" = ""
  )[colType]
  displaySuffix = c(
    "age" = " years old",
    "income" = "",
    "race" = ""
  )[colType]
  
  # MAKE THE MAP!
  leaflet() %>% 
    # BASE MAP
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    # COLOR IN
    addPolygons(
      data = ourMap,
      weight = 1,
      fillOpacity = .35,
      opacity = .375,
      color = ourClr, 
      # ignore this yucky line
      label = ~paste( columnFullName ,": ", displayPrefix, 
                      format(round(ourMap[[colName]])
                             , big.mark = ",", scientific = FALSE),
                      displaySuffix
      ),
      layerId = ~ourMap$PRECINCT
    ) %>% 
    # TITLE
    addControl(
      html = paste0(
        "<div style='
            font-size: 12px;
            font-weight:600;
            color:#000;
            background:rgba(255,255,255,0.9);
            padding:4px 10px;
            border-radius:6px;'>
          ", mapFullName , " | ", columnFullName ,"
         </div>"),
      position = "topright"
    ) %>% 
    # LEGEND
    addLegend(
      position = "bottomright",
      pal = palette,
      values = ourMap[[colName]],
      title = "Legend",
      opacity = 1
    )
}




#### District Functions ####

make_histogram_dist <- function(district="MUN", data="age"){
  
  if(data == "age"){
    
    if(district == "MUN"){
      df <- acs_interp_judicial
    }
    if(district == "CIT"){
      df <- acs_interp_cincy
    }
    if(district == "CPS"){
      df <- acs_interp_cps
    }
    
    if(district == "MUN"){
      word <- "Total Pop in Age Cohorts for Municiple Court, District 4"
    }
    if(district == "CIT"){
      word <- "Total Pop in Age Cohorts for City Council District"
    }
    if(district == "CPS"){
      word <- "Total Pop in Age Cohorts for CPS District"
    }
    
    `0-9` <- sum(as.numeric(df$age_0_9),na.rm = TRUE)
    `10-19` <- sum(as.numeric(df$age_10_19),na.rm = TRUE)
    `20-29` <- sum(as.numeric(df$age_20_29),na.rm = TRUE)
    `30-39` <- sum(as.numeric(df$age_30_39),na.rm = TRUE)
    `40-49` <- sum(as.numeric(df$age_40_49),na.rm = TRUE)
    `50-59` <- sum(as.numeric(df$age_50_59),na.rm = TRUE)
    `60-69` <- sum(as.numeric(df$age_60_69),na.rm = TRUE)
    `70-79` <- sum(as.numeric(df$age_70_79),na.rm = TRUE)
    `80+` <- sum(as.numeric(df$age_80plus),na.rm = TRUE)
    
    
    values <- c(`0-9`,`10-19`,`20-29`,`30-39`,`40-49`,`50-59`,`60-69`,`70-79`,`80+`)
    names(values) <- c("0-9", "10-19", "20-29", "30-39","40-49","50-59","60-69","70-79","80+")
    
    df_plot <- data.frame(
      age_group = names(values),
      population = values
    )
    
    plot <- ggplot(df_plot, aes(x = age_group, y = population)) +
      geom_bar(stat = "identity", fill = "#377eb8", show.legend = FALSE) +  # set your color here
      labs(title = word, x = "Group", y = "Population") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14) +
          scale_x_discrete(guide = guide_axis(position = "left"))
      )
    
  }
  
  if(data == "income"){
    
    
    if(district == "MUN"){
      df <- acs_interp_judicial
    }
    if(district == "CIT"){
      df <- acs_interp_cincy
    }
    if(district == "CPS"){
      df <- acs_interp_cps
    }
    
    if(district == "MUN"){
      word <- "Household Income Cohorts for Municiple Court, District 4"
    }
    if(district == "CIT"){
      word <- "Household Income Cohorts for City Council District"
    }
    if(district == "CPS"){
      word <- "Household Income Cohorts for CPS District"
    }
    
    `under25` <- sum(df$hhinc_under25k,na.rm = TRUE)
    `25_49` <- sum(df$hhinc_25_49k,na.rm = TRUE)
    `50_99` <- sum(df$hhinc_50_99k,na.rm = TRUE)
    `100_149` <- sum(df$hhinc_100_149k,na.rm = TRUE)
    `150_199` <- sum(df$hhinc_150_199k,na.rm = TRUE)
    `above200` <- sum(df$hhinc_200plus,na.rm = TRUE)
    
    
    values <- c(under25,`25_49`,`50_99`,`100_149`,
                `150_199`,above200)
    
    names(values) <- c("<25k", "25-49k", "50-99k", "100-149k",
                       "150-199k",">200k")
    
    df_plot <- data.frame(
      age_group = names(values),
      population = values
    )
    
    # Set factor levels in the desired order
    df_plot$age_group <- factor(df_plot$age_group,
                                levels = c("<25k", "25-49k", "50-99k", 
                                           "100-149k", "150-199k", ">200k"))
    
    plot <- ggplot(df_plot, aes(x = age_group, y = population)) +
      geom_bar(stat = "identity", fill = "#377eb8", show.legend = FALSE) +
      labs(title = word, x = "Group", y = "Population") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 15)
      )
    
  }
  
  if(data == "race"){
    
    if(district == "MUN"){
      df <- acs_interp_judicial
    }
    if(district == "CIT"){
      df <- acs_interp_cincy
    }
    if(district == "CPS"){
      df <- acs_interp_cps
    }
    
    if(district == "MUN"){
      word <- "Frequency of Races for Municiple Court, District 4"
    }
    if(district == "CIT"){
      word <- "Frequency of Races for City Council District"
    }
    if(district == "CPS"){
      word <- "Frequency of Races for CPS District"
    }
    
    `white` <- sum(df$whiteE,na.rm = TRUE)
    `black` <- sum(df$blackE,na.rm = TRUE)
    `asian` <- sum(df$asianE,na.rm = TRUE)
    `hispanic` <- sum(df$hispanicE,na.rm = TRUE)
    
    
    values <- c(white,`black`,`asian`,`hispanic`)
    
    names(values) <- c("White", "Black", "Asian", "Hispanic")
    
    df_plot <- data.frame(
      age_group = names(values),
      population = values
    )
    
    plot <- ggplot(df_plot, aes(x = age_group, y = population)) +
      geom_bar(stat = "identity", fill = "#377eb8", show.legend = FALSE) +  # set your color here
      labs(title = word, x = "Group", y = "Population") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14) +
          scale_x_discrete(guide = guide_axis(position = "left"))
      )
    
  }
  
  return(plot)
  
}

make_histogram_dist("CPS","age")

return_median_dist <- function(district="MUN", data="age"){
  
  
  if(district == "MUN"){
    df <- acs_interp_judicial
  }
  if(district == "CIT"){
    df <- acs_interp_cincy
  }
  if(district == "CPS"){
    df <- df_cps
  }
  
  if(data == "age"){
    
    return(mean(df$median_ageE))
    
  }
  
  if(data == "income"){
    
    return(mean(df$med_incomeE))
    
  }
  
  if(data == "race"){
    
    df$white_per <- df$whiteE/df$pop_totalE
    
    return_value <- mean(as.numeric(df$white_per))
    
    return(return_value)
    
  }
  
  
}

return_median_dist("CIT","race")

#### Precinct Functions ####

precinct_name <- function(district="MUN",code="0101 CIN 1-A"){
  
  if(district == "MUN"){
    df <- acs_interp_judicial
  }
  if(district == "CIT"){
    df <- acs_interp_cincy
  }
  if(district == "CPS"){
    df <- acs_interp_cps
  }
  
  df <- df[, -24]
  
  df_row <- df %>% 
    filter(PRECINCT == code)
  
  word <- df_row$PRECINCT
  
  result <- substring(word, 6)
  
  return(result)
  
}

make_histogram_pre <- function(district="MUN",code="0101 CIN 1-A", data="age"){
  
  if(district == "MUN"){
    df <- acs_interp_judicial
  }
  if(district == "CIT"){
    df <- acs_interp_cincy
  }
  if(district == "CPS"){
    df <- acs_interp_cps
  }
  
  df <- df[, -24]
  
  df_row <- df %>% 
    filter(PRECINCT == code)
  
  if(data == "age"){
    
    if(district == "MUN"){
      word <- paste("Total Pop in Age Cohorts for Precinct ",precinct_name(district,code),sep="")
    }
    if(district == "CIT"){
      word <- paste("Total Pop in Age Cohorts for Precinct ",precinct_name(district,code),sep="")
    }
    if(district == "CPS"){
      word <- paste("Total Pop in Age Cohorts for Precinct ",precinct_name(district,code),sep="")
    }
    
    `0-9` <- df_row$age_0_9
    `10-19` <- df_row$age_10_19
    `20-29` <- df_row$age_20_29
    `30-39` <- df_row$age_30_39
    `40-49` <- df_row$age_40_49
    `50-59` <- df_row$age_50_59
    `60-69` <- df_row$age_60_69
    `70-79` <- df_row$age_70_79
    `80+` <- df_row$age_80plus
    
    
    values <- c(`0-9`,`10-19`,`20-29`,`30-39`,`40-49`,`50-59`,`60-69`,`70-79`,`80+`)
    names(values) <- c("0-9", "10-19", "20-29", "30-39","40-49","50-59","60-69","70-79","80+")
    
    df_plot <- data.frame(
      age_group = names(values),
      population = values
    )
    
    plot <- ggplot(df_plot, aes(x = age_group, y = population)) +
      geom_bar(stat = "identity", fill = "#377eb8", show.legend = FALSE) +  # set your color here
      labs(title = word, x = "Group", y = "Population") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 15)
      )
    
  }
  
  if(data == "income"){
    
    if(district == "MUN"){
      word <- paste("Household Income Cohorts for Precinct ",precinct_name(district,code),sep="")
    }
    if(district == "CIT"){
      word <- paste("Household Income Cohorts for Precinct ",precinct_name(district,code),sep="")
    }
    if(district == "CPS"){
      word <- paste("Household Income Cohorts for Precinct ",precinct_name(district,code),sep="")
    }
    
    `under25` <- df_row$hhinc_under25k
    `25_49` <- df_row$hhinc_25_49k
    `50_99` <- df_row$hhinc_50_99k
    `100_149` <- df_row$hhinc_100_149k
    `150_199` <- df_row$hhinc_150_199k
    `above200` <- df_row$hhinc_200plus
    
    
    values <- c(`under25`,`25_49`,`50_99`,`100_149`,
                `150_199`,`above200`)
    
    names(values) <- c("<25k", "25-49k", "50-99k", "100-149k",
                       "150-199k",">200k")
    
    df_plot <- data.frame(
      age_group = names(values),
      population = values
    )
    
    # Set factor levels in the desired order
    df_plot$age_group <- factor(df_plot$age_group,
                                levels = c("<25k", "25-49k", "50-99k", 
                                           "100-149k", "150-199k", ">200k"))
    
    plot <- ggplot(df_plot, aes(x = age_group, y = population)) +
      geom_bar(stat = "identity", fill = "#377eb8", show.legend = FALSE) +
      labs(title = word, x = "Group", y = "Population") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 15)
      )
    
  }
  
  if(data == "race"){
    
    if(district == "MUN"){
      word <- paste("Frequency of Races for Precinct ",precinct_name(district,code),sep="")
    }
    if(district == "CIT"){
      word <- paste("Frequency of Races for Precinct ",precinct_name(district,code),sep="")
    }
    if(district == "CPS"){
      word <- paste("Frequency of Races for Precinct ",precinct_name(district,code),sep="")
    }
    
    `white` <- df_row$whiteE
    `black` <- df_row$blackE
    `asian` <- df_row$asianE
    `hispanic` <- df_row$hispanicE
    
    
    values <- c(white,`black`,`asian`,`hispanic`)
    
    names(values) <- c("White", "Black", "Asian", "Hispanic")
    
    df_plot <- data.frame(
      age_group = names(values),
      population = values
    )
    
    plot <- ggplot(df_plot, aes(x = age_group, y = population)) +
      geom_bar(stat = "identity", fill = "#377eb8", show.legend = FALSE) +  # set your color here
      labs(title = word, x = "Group", y = "Population") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 15)
      )
    
  }
  
  return(plot)
  
}

make_histogram_pre("MUN","0101 CIN 1-A","income")

truncate_to_2 <- function(x) {
  floor(x * 100) / 100
}

return_median_pre <- function(district="MUN",code="0101 CIN 1-A", data="age"){
  
  if(district == "MUN"){
    df <- acs_interp_judicial
  }
  if(district == "CIT"){
    df <- acs_interp_cincy
  }
  if(district == "CPS"){
    df <- acs_interp_cps
  }
  
  df <- df[, -24]
  
  df_row <- df %>% 
    filter(PRECINCT == code)
  
  if(data == "age"){
    return(df_row$median_ageE)
  }
  
  if(data == "income"){
    return(df_row$med_incomeE)
  }
  
  if(data == "race"){
    
    df_row$white_per <- df_row$whiteE/df_row$pop_totalE
    
    return_value <- as.numeric(df_row$white_per)
    return_value <- return_value * 100
    return_value <- truncate_to_2(return_value)
    return_value <- paste(return_value,"%",sep = "")
    
    
    return(return_value)
  }
  
}

return_median_pre("CPS","2203 CIN 22-C","race")



precinct_name("MUN","0101 CIN 1-A")
####Shiny code ####
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
  
  titlePanel("Demographic Map"),
  
  # row for dropdowns
  fluidRow(
    column(
      width = 3,
      selectInput(inputId = "map_dropdown", 
                  label = "Choose a Campaign", 
                  choices = c("School Board" = "CPS",
                              "City Council" = "CIT",
                              "Judge" = "MUN"),
                  selected = "CPS")   
    ),
    column(
      width = 3,
      selectInput(inputId = "data_dropdown", 
                  label = "Choose a Demographic", 
                  choices = c("Age" = "age",
                              "Income" = "income",
                              "race" = "race"),
                  selected = "age") 
    )
  ),
  
  # map output with spinner
  fluidRow(
    column( width = 6,
      div(
      class = "map",
      h3("Precinct Map"),
      withSpinner(
        leafletOutput("map_result", height = 600), 
        type = 8, color = "#7B4BCC", proxy.height = 200
      )
    )
  ),#end column
  column( width = 6,
    div(
      class = "Graph Display",
      withSpinner(
        plotOutput("test1"),
        type = 8, color = "#7B4BCC", proxy.height = 400
      ),
      withSpinner(
        plotOutput("test2"),
        type = 8, color = "#7B4BCC", proxy.height = 400
      )
    )
  )
  )
)

server <- function(input, output, session) {
  
  # helper: map choice -> dataset object name
  mapDict = list(
    "MUN" = "acs_interp_judicial",
    "CIT" = "acs_interp_cincy",
    "CPS" = "acs_interp_cps"
  )
  
  # returns vector of precinct ids for the given map choice
  get_precinct_list <- function(map_choice) {
    ds_name <- mapDict[[as.character(map_choice)]]
    if (is.null(ds_name)) return(character(0))
    df <- tryCatch(get(ds_name), error = function(e) NULL)
    if (is.null(df)) return(character(0))
    # if your data had a junk column 24 you previously removed, be safe:
    if (ncol(df) >= 24) {
      # don't drop here globally, just ensure PRECINCT exists
      # df <- df[, -24]
      invisible(NULL)
    }
    precincts <- unique(as.character(df$PRECINCT))
    precincts <- precincts[!is.na(precincts)]
    return(precincts)
  }
  
  # Render leaflet map (unchanged)
  output$map_result <- renderLeaflet({
    shiny.map(input$map_dropdown, input$data_dropdown)
  })
  
  # district-level plot
  output$test1 <- renderPlot({
    make_histogram_dist(input$map_dropdown, input$data_dropdown)
  })
  
  # precinct-level plot (uses clicked precinct if valid; otherwise uses a sensible default)
  output$test2 <- renderPlot({
    click <- input$map_result_shape_click
    precincts <- get_precinct_list(input$map_dropdown)
    
    # Determine chosen precinct id:
    Pre <- NULL
    if (!is.null(click) && !is.null(click$id)) {
      # sometimes click$id can be a list/number; coerce to character
      clicked_id <- as.character(click$id)
      if (clicked_id %in% precincts) {
        Pre <- clicked_id
      } else {
        # clicked id not in current precinct list (e.g., from previous map); ignore it
        Pre <- NULL
      }
    }
    
    # If we don't have a valid clicked precinct, pick a default (first precinct in dataset)
    if (is.null(Pre)) {
      if (length(precincts) > 0) {
        Pre <- precincts[1]
      } else {
        # ultimate fallback: a known code for each map (keeps backward compatibility)
        fallback <- c("MUN" = "0601 CIN 6-A", "CIT" = "0101 CIN 1-A", "CPS" = "2203 CIN 22-C")
        Pre <- fallback[[input$map_dropdown]]
      }
    }
    
    # Now draw the precinct histogram
    make_histogram_pre(input$map_dropdown, Pre, input$data_dropdown)
  })
}

# run app
shinyApp(ui, server)
