#### Packages ####

library(tidyverse)

#### Data ####

df_judge <- read_csv("../data/Acs_Data.csv")

df_council <- read_csv("../data/Acs_Data.csv")

df_cps <- read_csv("../data/Acs_Data.csv")

#### Functions ####

make_histogram <- function(district="judge", data="age"){
  
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
  
  df_plot <- data.frame(
    age_group = names(values),
    population = values
  )
  
  ggplot(df_plot, aes(x = age_group, y = population, fill = age_group)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    labs(title = word, x = "Age Group", y = "Population") +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 15)   # reduce title font size
    ) +
    scale_fill_brewer(palette = "Blues")
}

make_histogram("council")










