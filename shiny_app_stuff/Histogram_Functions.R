#### Packages ####

library(tidyverse)

#### Data ####

df_judge <- read_csv("../data/Acs_Data.csv")

df_council <- read_csv("../data/Acs_Data.csv")

df_cps <- read_csv("../data/Acs_Data.csv")

#### District Functions ####

make_histogram_dist <- function(district="judge", data="age"){
  
  if(data == "age"){
  
  
  if(district == "judge"){
  df <- df_judge
}
  if(district == "council"){
    df <- df_council
  }
  if(district == "cps"){
    df <- df_cps
  }
  
  if(district == "judge"){
    word <- "Total Pop in Age Cohorts for Municiple Court, District 4"
  }
  if(district == "council"){
    word <- "Total Pop in Age Cohorts for City Council District"
  }
  if(district == "cps"){
    word <- "Total Pop in Age Cohorts for CPS District"
  }

  `0-9` <- sum(df$age_0_9)
  `10-19` <- sum(df$age_10_19)
  `20-29` <- sum(df$age_20_29)
  `30-39` <- sum(df$age_30_39)
  `40-49` <- sum(df$age_40_49)
  `50-59` <- sum(df$age_50_59)
  `60-69` <- sum(df$age_60_69)
  `70-79` <- sum(df$age_70_79)
  `80+` <- sum(df$age_80plus)
  
  
  values <- c(`0-9`,`10-19`,`20-29`,`30-39`,`40-49`,`50-59`,`60-69`,`70-79`,`80+`)
  names(values) <- c("0-9", "10-19", "20-29", "30-39","40-49","50-59","60-69","70-79","80+")
  
  }
  
  if(data == "income"){
    
    
    if(district == "judge"){
      df <- df_judge
    }
    if(district == "council"){
      df <- df_council
    }
    if(district == "cps"){
      df <- df_cps
    }
    
    if(district == "judge"){
      word <- "Household Income Cohorts for Municiple Court, District 4"
    }
    if(district == "council"){
      word <- "Household Income Cohorts for City Council District"
    }
    if(district == "cps"){
      word <- "Household Income Cohorts for CPS District"
    }
    
    `under25` <- sum(df$hhinc_under25k)
    `25_49` <- sum(df$hhinc_25_49k)
    `50_99` <- sum(df$hhinc_50_99k)
    `100_149` <- sum(df$hhinc_100_149k)
    `150_199` <- sum(df$hhinc_150_199k)
    `above200` <- sum(df$hhinc_200plus)
    
    
    values <- c(under25,`25_49`,`50_99`,`100_149`,
                `150_199`,above200)
   
     names(values) <- c("<25k", "25-49k", "50-99k", "100-149k",
                        "150-199k",">200k")
    
  }
  
  if(data == "race"){
      
      if(district == "judge"){
        df <- df_judge
      }
      if(district == "council"){
        df <- df_council
      }
      if(district == "cps"){
        df <- df_cps
      }
      
      if(district == "judge"){
        word <- "Frequency of Races for Municiple Court, District 4"
      }
      if(district == "council"){
        word <- "Frequency of Races for City Council District"
      }
      if(district == "cps"){
        word <- "Frequency of Races for CPS District"
      }
      
      `white` <- sum(df$whiteE)
      `black` <- sum(df$blackE)
      `asian` <- sum(df$asianE)
      `hispanic` <- sum(df$hispanicE)
      
      
      values <- c(white,`black`,`asian`,`hispanic`)
      
      names(values) <- c("White", "Black", "Asian", "Hispanic")

  }
  
  df_plot <- data.frame(
    age_group = names(values),
    population = values
  )
  
  ggplot(df_plot, aes(x = age_group, y = population)) +
    geom_bar(stat = "identity", fill = "#377eb8", show.legend = FALSE) +  # set your color here
    labs(title = word, x = "Group", y = "Population") +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 15)
    )
}

make_histogram_dist("cps","income")

return_median_dist <- function(district="judge", data="age"){
  
  
  if(district == "judge"){
    df <- df_judge
  }
  if(district == "council"){
    df <- df_council
  }
  if(district == "cps"){
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

return_median_dist("council","race")
  
#### Precinct Functions ####

make_histogram_pre <- function(district="judge",precint=1, data="age"){
  
  if(district == "judge"){
    df <- df_judge
  }
  if(district == "council"){
    df <- df_council
  }
  if(district == "cps"){
    df <- df_cps
  }
  
  df_row <- df %>% 
    filter(id == precint)
  
  if(data == "age"){
    
    if(district == "judge"){
      word <- paste("Total Pop in Age Cohorts for Precint ",precint,sep="")
    }
    if(district == "council"){
      word <- paste("Total Pop in Age Cohorts for Precint ",precint,sep="")
    }
    if(district == "cps"){
      word <- paste("Total Pop in Age Cohorts for Precint ",precint,sep="")
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
    
  }
    
  if(data == "income"){
    
    if(district == "judge"){
      word <- paste("Household Income Cohorts for Precint ",precint,sep="")
    }
    if(district == "council"){
      word <- paste("Household Income Cohorts for Precint ",precint,sep="")
    }
    if(district == "cps"){
      word <- paste("Household Income Cohorts for Precint ",precint,sep="")
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
    
  }
  
  if(data == "race"){
    
    if(district == "judge"){
      word <- paste("Frequency of Races for Precint ",precint,sep="")
    }
    if(district == "council"){
      word <- paste("Frequency of Races for Precint ",precint,sep="")
    }
    if(district == "cps"){
      word <- paste("Frequency of Races for Precint ",precint,sep="")
    }
    
    `white` <- df_row$whiteE
    `black` <- df_row$blackE
    `asian` <- df_row$asianE
    `hispanic` <- df_row$hispanicE
    
    
    values <- c(white,`black`,`asian`,`hispanic`)
    
    names(values) <- c("White", "Black", "Asian", "Hispanic")
    
  }
  
  df_plot <- data.frame(
    age_group = names(values),
    population = values
  )
  
  ggplot(df_plot, aes(x = age_group, y = population)) +
    geom_bar(stat = "identity", fill = "#377eb8", show.legend = FALSE) +  # set your color here
    labs(title = word, x = "Group", y = "Population") +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 15)
    )
  
}

make_histogram_pre("judge",2,"income")




















