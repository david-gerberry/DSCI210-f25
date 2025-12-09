library(tidyverse)








df <- read_csv("Salary_Data.csv")


df <- df %>% 
  mutate(
    `Education Level` = `Education Level` %>%
      str_remove(" Degree")) %>% 
  subset(`Education Level` != "NA")

df[5876,3] <- "PhD"    



df %>% 
  ggplot(aes(y = Salary,x = `Years of Experience`))+
  geom_point()+
  facet_wrap(~ df$`Education Level`)








