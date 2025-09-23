#### Packages ####

library(tidyverse)

#### Data ####

load("data/acs_data.RData")

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

precinct_name("MUN","0101 CIN 1-A")

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













