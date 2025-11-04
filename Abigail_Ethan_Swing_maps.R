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

png("Abigail_Pct_votes_silverstein.png", width = 700, height = 500)
ggplot(data = precincts_votes) +
  geom_sf(aes(fill = pct_silverstein), color = "white", size = 0.2) +
  geom_sf(data = boundary, fill = NA, color = "black", size = 0.5) +
  scale_fill_gradient(
    low = "red", high = "blue",
    na.value = "grey90",
    name = "Pct Silverstein"
  )  +
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
dev.off()


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

png("Abigail_Pct_votes_berkowitz.png", width = 700, height = 500)
ggplot(data = precincts_berk_votes) +
  geom_sf(aes(fill = pct_berkowitz), color = "white", size = 0.2) +
  geom_sf(data = boundary, fill = NA, color = "black", size = 0.5) +
  scale_fill_gradient(
    low = "blue", high = "red",
    na.value = "grey90",
    name = "Pct Berkowitz"
  )  +
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
dev.off()


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

png("Abigail_swing_silverstein.png", width = 700, height = 500)
ggplot(precincts_swing) +
  geom_sf(aes(fill = pct_rep), color = "black", size = 0.2) +
  geom_sf(data = boundary, fill = NA, color = "black", size = 0.5) +
  scale_fill_gradientn(
    colours = rev(RColorBrewer::brewer.pal(n = 10, name = "RdBu")),
    na.value = "transparent",
    values = c(0, 0.5, 1),
    breaks = c(0, 0.5, 1),
    labels = c("100% Silvertein", "50%", "100% Kissinger"),
    limits = c(0, 1)
  ) +
  labs(
    title = "2023 Judicial Election District 4",
    subtitle = "Curt Kissinger vs Samantha Silverstein",
    fill = "Support Level"
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
dev.off()

precincts_swing$pct_rep <- precincts_swing$pct_rep / 100



#base/swing map #2019 

results2019 <- read_excel("data/election results/G19_Official_Canvass.xlsx", 
                          sheet = "Judicial",skip=2)
mapANDresults2019 <-
  left_join(map2020, results2019, by = c("PRECINCT" = "PRECINCT")) %>% 
  filter( !is.na(`Josh Berkowitz`) )

png("Abigail_swing_berkowitz.png", width = 700, height = 500)
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
dev.off()



ggplot(precincts_berk_votes) +
  geom_sf(aes(fill = pct_berkowitz), color = "black", size = 0.2) +
  geom_sf(data = boundary, fill = NA, color = "black", size = 0.5) +
  scale_fill_gradientn(
    colours = rev(RColorBrewer::brewer.pal(n = 10, name = "RdBu")),
    na.value = "transparent",
    values = c(0, 0.5, 1),
    breaks = c(0, 0.5, 1),
    labels = c("100% Kennedy", "50%", "100% Berkowitz"),
    limits = c(0, 1)
  ) +
  labs(
    title = "2019 Judicial Election District 4",
    subtitle = "Josh Berkowitz vs John Kennedy",
    fill = "Support Level"
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

precincts_berk_votes$pct_berkowitz <- precincts_berk_votes$pct_berkowitz / 100


#combined

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
    w = 0.5,  # weight on 2019
    weighted_rep = w * pct_rep_2019 + (1 - w) * pct_rep_2023,
    category = case_when(
      weighted_rep >= 60 ~ "Base (Red)",
      weighted_rep >= 40 ~ "Swing (Yellow)",
      weighted_rep < 40  ~ "Residual (Blue)",
      TRUE ~ "Missing"
    )
  )
combined_w$weighted_rep <- combined_w$weighted_rep / 100

combined_w$weighted_rep[is.na(combined_w$weighted_rep)] <- 
  silverstein_2023$PERCENT[is.na(combined_w$weighted_rep)]

combined_w$weighted_rep <- as.numeric(as.character(combined_w$weighted_rep))


# Map
cincy.neighborhoods <- st_zm(st_read("data/maps/snabnd_2010.shp"))
oakley <- cincy.neighborhoods[grepl('Oakley', cincy.neighborhoods$SNA_NAME),]
linwood <- cincy.neighborhoods[grepl('Linwood', cincy.neighborhoods$SNA_NAME),]


png("Abigail_swing_combined_neighborhoods.png", width = 700, height = 500)
ggplot(combined_w) +
  geom_sf(aes(fill = category), color = "black", size = 0.2) +
  geom_sf(data = boundary, fill = NA, color = "black", size = 0.5) +
  scale_fill_manual(
    values = c("Base (Red)" = "red",
               "Swing (Yellow)" = "yellow",
               "Residual (Blue)" = "blue",
               "Missing" = "blue"),
               breaks = c("Base (Red)", "Swing (Yellow)", "Residual (Blue)")  # exclude Missing from legend)
  ) +
  theme_minimal() +
  labs(
    title = "Weighted Base/Swing Map",
    subtitle = "50% weight to 2019, 50% to 2023",
    fill = "Category"
  ) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  geom_sf(data = oakley,col='pink',fill=NA,lwd=1.5)+
  geom_sf_label(data=oakley,aes(label = 'Oakley'), cex=2.5, position = position_nudge(x=.03, y=-.005)) +
  geom_sf(data = linwood,col='orange',fill=NA,lwd=1.5)+
  geom_sf_label(data=linwood,aes(label = 'Linwood'), cex=2.5, position = position_nudge(x=.005, y=-.008))
dev.off()

ggplot(combined_w) +
  geom_sf(aes(fill = weighted_rep), color = "black", size = 0.2) +
  geom_sf(data = boundary, fill = NA, color = "black", size = 0.5) +
  scale_fill_gradientn(
    colours = rev(RColorBrewer::brewer.pal(n = 10, name = "RdBu")),
    na.value = "transparent",
    values = c(0, 0.5, 1),
    breaks = c(0, 0.5, 1),
    labels = c("0%", "50%", "100%"),
    limits = c(0, 1)
  ) +
  labs(
    title = "Weighted Base/Swing Map Judicial Election District 4",
    subtitle = "50% weight to Berkowitz 2019, 50% to Kissinger 2023",
    fill = "Support Level"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)
  ) +
  geom_sf(data = oakley, col = 'pink', fill = NA, lwd = 1.5) +
  geom_sf_label(
    data = oakley,
    aes(label = 'Oakley'),
    cex = 2.5,
    position = position_nudge(x = .03, y = -0.005)
  ) +
  geom_sf(data = linwood, col = 'orange', fill = NA, lwd = 1.5) +
  geom_sf_label(
    data = linwood,
    aes(label = 'Linwood'),
    cex = 2.5,
    position = position_nudge(x = .005, y = -0.008)
  )
