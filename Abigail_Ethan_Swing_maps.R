library(tidyverse)
library(sf)
library(readxl)
library(RColorBrewer)

#silverstein
silverstein_2023 <- read_excel("data/election results/G23_Official_Canvass.xlsx")
colnames(silverstein_2023) <- silverstein_2023[2, ]
silverstein_2023 <- silverstein_2023[-c(1,2), ]

precincts <- st_read("shapefiles/judicial_precincts.shp")
boundary  <- st_read("shapefiles/judicial_boundary.shp")

silverstein_2023 <- silverstein_2023 %>%
  mutate(`PRC #` = substr(PRECINCT, 1, 4))

silverstein_2023 <- silverstein_2023 %>%
  mutate(
    `Samantha Silverstein` = as.numeric(`Samantha Silverstein`),
    `Curt           Kissinger` = as.numeric(`Curt           Kissinger`)
  )

silverstein_2023 <- silverstein_2023 %>%
  mutate(
    # adjust these column names to your actual ones
    total_votes = `Samantha Silverstein` + `Curt           Kissinger`,  
    pct_silverstein = 100 * `Samantha Silverstein` / total_votes
  )


precincts_votes <-precincts %>%
  left_join(silverstein_2023, by = c("PRECINCT" = "PRC #"))

ggplot(data = precincts_votes) +
  geom_sf(aes(fill = pct_silverstein), color = "white", size = 0.2) +
  geom_sf(data = boundary, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(na.value = "grey90", option = "plasma",
                       name = "Pct Silverstein") +
  theme_void() +
  labs(
    title = "Percentage of Votes for Silverstein by Precinct",
    subtitle = "Each precinct shaded by % of votes cast for Silverstein",
    fill = "% Silverstein"
  ) +
  theme(
    axis.text = element_blank(),      # remove numbers
    axis.ticks = element_blank()
  )  

#berkowits 2019
berkowitz_2019 <- read_excel("data/election results/G19_Official_Canvass.xlsx")
colnames(berkowitz_2019) <- berkowitz_2019[2, ]
berkowitz_2019 <- berkowitz_2019[-c(1,2), ]

berkowitz_2019 <- berkowitz_2019 %>%
  mutate(`PRC #` = substr(PRECINCT, 1, 4))

berkowitz_2019 <- berkowitz_2019 %>%
  mutate(
    `Josh Berkowitz` = as.numeric(`Josh Berkowitz`),
    `John Kennedy` = as.numeric(`John Kennedy`)
  )

berkowitz_2019 <- berkowitz_2019 %>%
  mutate(
    # adjust these column names to your actual ones
    total_votes = `Josh Berkowitz` + `John Kennedy`,  
    pct_berkowitz = 100 * `Josh Berkowitz` / total_votes
  )


precincts_berk_votes <-precincts %>%
  left_join(berkowitz_2019, by = c("PRECINCT" = "PRC #"))

ggplot(data = precincts_berk_votes) +
  geom_sf(aes(fill = pct_berkowitz), color = "white", size = 0.2) +
  geom_sf(data = boundary, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(na.value = "grey90", option = "plasma",
                       name = "Pct Berkowitz") +
  theme_void() +
  labs(
    title = "Percentage of Votes for Berkowitz by Precinct",
    subtitle = "Each precinct shaded by % of votes cast for Berkowitz",
    fill = "% Silverstein"
  ) +
  theme(
    axis.text = element_blank(),      # remove numbers
    axis.ticks = element_blank()
  )  

#base/swing map #2023
precincts_swing <- precincts %>%
  left_join(silverstein_2023 %>% 
              select(`PRC #`, `Curt           Kissinger`, `Samantha Silverstein`), 
            by = c("PRECINCT" = "PRC #")) %>%
  mutate(
    total_votes = `Curt           Kissinger` + `Samantha Silverstein`,
    pct_rep = 100 * `Curt           Kissinger` / total_votes,
    category = case_when(
      pct_rep >= 60 ~ "Base (Red)",      # Strong Republican
      pct_rep >= 40 & pct_rep < 60 ~ "Swing (Yellow)",  # Competitive
      pct_rep < 40 ~ "Residual (Blue)",  # Strong Democratic
      TRUE ~ "Missing"
    )
  )
ggplot(precincts_swing) +
  geom_sf(aes(fill = category), color = "black", size = 0.2) +
  geom_sf(data = boundary, fill = NA, color = "black", size = 0.5) +
  scale_fill_manual(values = c(
    "Base (Red)" = "red",
    "Swing (Yellow)" = "yellow",
    "Residual (Blue)" = "blue"
  )) +
  theme_void() +
  labs(
    title = "2023 Judicial Election District 4",
    subtitle = "Curt Kissinger vs Samantha Silverstein",
    fill = "Category"
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

#base/swing map #2019 

results2019 <- read_excel("data/election results/G19_Official_Canvass.xlsx", 
                          sheet = "Judicial",skip=2)
mapANDresults2019 <-
  left_join(map2020, results2019, by = c("PRECINCT" = "PRECINCT")) %>% 
  filter( !is.na(`Josh Berkowitz`) )

mapANDresults2019 %>% 
  mutate(Rep.prop = `Josh Berkowitz`/( `Josh Berkowitz`+ `John Kennedy`)) %>%
  mutate(Rep.baseswing = cut(Rep.prop, breaks = c(-0.001, 0.2,.40, 0.60, 0.8,1),labels = c("Very Residual","Residual", "Swing", "Base","Very Base")))%>%
  ggplot(aes(fill=Rep.baseswing)) +
  geom_sf()+
  labs(title = "2019 Judicial Election District 4", 
       subtitle = "John Kennedy vs Josh Berkowitz",
       fill = "Vote for \nBerkowitz (%)", 
       caption = "")+
  scale_fill_manual(
    values = c(
      "Very Residual" = "blue",
      "Residual" = "blue",
      "Base" = "red",
      "Very Base" = "darkred",
      
      "Swing" = "yellow"
    )
  ) +
  theme_void()

#combined
results2019 <- results2019 %>%
  mutate(
    total2019 = `Josh Berkowitz` + `John Kennedy`,
    pct_rep_2019 = 100 * `Josh Berkowitz` / total2019
  )
precincts_swing <- precincts %>%
  left_join(silverstein_2023 %>% 
              select(`PRC #`, `Curt           Kissinger`, `Samantha Silverstein`), 
            by = c("PRECINCT" = "PRC #")) %>%
  mutate(
    total2023 = `Curt           Kissinger` + `Samantha Silverstein`,
    pct_rep_2023 = 100 * `Curt           Kissinger` / total2023
  )
combined <- precincts_swing %>%
  left_join(
    results2019 %>% select(PRECINCT, pct_rep_2019),
    by = c("PRECINCT" = "PRECINCT")
  )
combined <- combined %>%
  mutate(
    category = case_when(
      pct_rep_2023 >= 60 & pct_rep_2019 >= 60 ~ "Base (Red)",
      pct_rep_2023 < 40 & pct_rep_2019 < 40 ~ "Residual (Blue)",
      TRUE ~ "Swing (Yellow)"
    )
  )

ggplot(combined) +
  geom_sf(aes(fill = category), color = "white", size = 0.2) +
  geom_sf(data = boundary, fill = NA, color = "black", size = 0.5) +
  scale_fill_manual(values = c(
    "Base (Red)" = "red",
    "Swing (Yellow)" = "yellow",
    "Residual (Blue)" = "blue"
  )) +
  theme_minimal() +
  labs(
    title = "Combined Base / Swing / Residual Precincts",
    subtitle = "2019 Berkowitz vs 2023 Kissinger",
    fill = "Category"
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

#combined weighted
precincts <- precincts %>% mutate(PRC = substr(PRECINCT, 1, 4))

silverstein_2023 <- silverstein_2023 %>%
  mutate(PRC = substr(PRECINCT, 1, 4),
         `Samantha Silverstein` = as.numeric(`Samantha Silverstein`),
         `Curt           Kissinger` = as.numeric(`Curt           Kissinger`),
         total2023 = `Samantha Silverstein` + `Curt           Kissinger`,
         pct_rep_2023 = 100 * `Curt           Kissinger` / total2023)

berkowitz_2019 <- berkowitz_2019 %>%
  mutate(PRC = substr(PRECINCT, 1, 4),
         `Josh Berkowitz` = as.numeric(`Josh Berkowitz`),
         `John Kennedy` = as.numeric(`John Kennedy`),
         total2019 = `Josh Berkowitz` + `John Kennedy`,
         pct_rep_2019 = 100 * `Josh Berkowitz` / total2019)

# Combine
combined_w <- precincts %>%
  left_join(select(berkowitz_2019, PRC, pct_rep_2019), by = "PRC") %>%
  left_join(select(silverstein_2023, PRC, pct_rep_2023), by = "PRC") %>%
  mutate(
    w = 0.7,  # weight on 2019
    weighted_rep = w * pct_rep_2019 + (1 - w) * pct_rep_2023,
    category = case_when(
      weighted_rep >= 60 ~ "Base (Red)",
      weighted_rep >= 40 ~ "Swing (Yellow)",
      weighted_rep < 40  ~ "Residual (Blue)",
      TRUE ~ "Missing"
    )
  )

# Map
ggplot(combined_w) +
  geom_sf(aes(fill = category), color = "black", size = 0.2) +
  geom_sf(data = boundary, fill = NA, color = "black", size = 0.5) +
  scale_fill_manual(
    values = c("Base (Red)" = "red",
               "Swing (Yellow)" = "yellow",
               "Residual (Blue)" = "blue",
               "Missing" = "grey90")
  ) +
  theme_minimal() +
  labs(
    title = "Weighted Base/Swing Map",
    subtitle = "70% weight to 2019, 30% to 2023",
    fill = "Category"
  ) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

