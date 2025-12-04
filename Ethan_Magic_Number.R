#### Libraries ####

library(tidyverse)
library(readxl)
library(scales)

#### Data ####

#load("data/acs_data.RData")

rm(acs_interp_cincy)
rm(acs_interp_cps)
rm(acs_interp_ham)

data_25 <- read_excel("data/detail.xlsx", sheet = 2)
data_24 <- read_excel("data/election results/G24_Official_Canvass.xlsx")
data_23 <- read_excel("data/election results/G23_Official_Canvass.xlsx")
data_22 <- read_excel("data/election results/G22_Official_Canvass.xlsx")
data_21 <- read_excel("data/election results/G21_Official_Canvass.xlsx")
data_20 <- read_excel("data/election results/G20_Official_Canvass.xlsx")
data_19 <- read_excel("data/election results/G19_Official_Canvass.xlsx")
data_18 <- read_excel("data/election results/G18_Official_Amended_Canvass.xlsx")
data_17 <- read_excel("data/election results/G17OFFCAN.xls")
data_16 <- read_excel("data/election results/Gen16OffCanvass.xlsx")
data_15 <- read_excel("data/election results/G15OFFCAN.xls")
data_14 <- read_excel("data/election results/G14AuditOffCanvass.xls")
data_13 <- read_excel("data/election results/G13OFFCANVASS.xls")

name_columns <- function(df) {
  colnames(df) <- as.character(unlist(df[1, ]))  
  df <- df[-1, ]                                
  return(df)
}

data_24 <- name_columns(data_24)
data_23 <- name_columns(data_23)
data_23 <- name_columns(data_23)
data_22 <- name_columns(data_22)
data_21 <- name_columns(data_21)
data_21 <- name_columns(data_21)
data_20 <- name_columns(data_20)
data_19 <- name_columns(data_19)
data_19 <- name_columns(data_19)
data_18 <- name_columns(data_18)
data_18 <- name_columns(data_18)
data_17 <- name_columns(data_17)
data_17 <- name_columns(data_17)
data_16 <- name_columns(data_16)
data_16 <- name_columns(data_16)
data_15 <- name_columns(data_15)
data_15 <- name_columns(data_15)
data_14 <- name_columns(data_14)
data_14 <- name_columns(data_14)
data_13 <- name_columns(data_13)
data_13 <- name_columns(data_13)

#### Cleaning ####

data_25 <- data_25 %>%
  filter(Precinct %in% acs_interp_judicial$PRECINCT)

data_18 <- data_18[ , c(2, 3, 4)]

data_18 <- data_18 %>%
  filter(PRECINCT %in% acs_interp_judicial$PRECINCT)

data_19 <- data_19[ , c(2, 3, 4, 12, 13)]

data_19 <- data_19 %>%
  filter(PRECINCT %in% acs_interp_judicial$PRECINCT)

data_20 <- data_20[ , c(2, 3, 4)]

data_20 <- data_20 %>%
  filter(PRECINCT %in% acs_interp_judicial$PRECINCT)

data_21 <- data_21[ , c(2, 3, 4)]

data_21 <- data_21 %>%
  filter(PRECINCT %in% acs_interp_judicial$PRECINCT)

data_22 <- data_22[ , c(2, 3, 4)]

data_22 <- data_22 %>%
  filter(PRECINCT %in% acs_interp_judicial$PRECINCT)

data_23 <- data_23[ , c(2, 3, 4, 9, 10)]

data_23 <- data_23 %>%
  filter(PRECINCT %in% acs_interp_judicial$PRECINCT)

data_24 <- data_24[ , c(2, 3, 4)]

data_24 <- data_24 %>%
  filter(PRECINCT %in% acs_interp_judicial$PRECINCT)

data_17 <- data_17[ , c(2, 3, 4, 11, 12)]

data_17 <- data_17 %>%
  filter(PRECINCT %in% acs_interp_judicial$PRECINCT)

data_16 <- data_16[ , c(2, 3, 4)]

data_16 <- data_16 %>%
  filter(PRECINCT %in% acs_interp_judicial$PRECINCT)

data_15 <- data_15[ , c(2, 3, 4, 8, 9)]

data_15 <- data_15 %>%
  filter(PRECINCT %in% acs_interp_judicial$PRECINCT)

data_14 <- data_14[ , c(2, 3, 4)]

data_14 <- data_14 %>%
  filter(PRECINCT %in% acs_interp_judicial$PRECINCT)

data_13 <- data_13[ , c(2, 3, 4)]

data_13 <- data_13 %>%
  filter(PRECINCT %in% acs_interp_judicial$PRECINCT)




data_18$`BALLOTS CAST TOTAL` <- as.numeric(data_18$`BALLOTS CAST TOTAL`)
data_18$`REGISTERED VOTERS TOTAL` <- as.numeric(data_18$`REGISTERED VOTERS TOTAL`)

data_19$`BALLOTS CAST TOTAL` <- as.numeric(data_19$`BALLOTS CAST TOTAL`)
data_19$`REGISTERED VOTERS TOTAL` <- as.numeric(data_19$`REGISTERED VOTERS TOTAL`)

data_20$`BALLOTS CAST TOTAL` <- as.numeric(data_20$`BALLOTS CAST TOTAL`)
data_20$`REGISTERED VOTERS TOTAL` <- as.numeric(data_20$`REGISTERED VOTERS TOTAL`)

data_21$`BALLOTS CAST TOTAL` <- as.numeric(data_21$`BALLOTS CAST TOTAL`)
data_21$`REGISTERED VOTERS TOTAL` <- as.numeric(data_21$`REGISTERED VOTERS TOTAL`)

data_22$`BALLOTS CAST TOTAL` <- as.numeric(data_22$`BALLOTS CAST TOTAL`)
data_22$`REGISTERED VOTERS TOTAL` <- as.numeric(data_22$`REGISTERED VOTERS TOTAL`)

data_23$`BALLOTS CAST TOTAL` <- as.numeric(data_23$`BALLOTS CAST TOTAL`)
data_23$`REGISTERED VOTERS TOTAL` <- as.numeric(data_23$`REGISTERED VOTERS TOTAL`)

data_24$`BALLOTS CAST TOTAL` <- as.numeric(data_24$`BALLOTS CAST TOTAL`)
data_24$`REGISTERED VOTERS TOTAL` <- as.numeric(data_24$`REGISTERED VOTERS TOTAL`)

data_17$`BALLOTS CAST TOTAL` <- as.numeric(data_17$`BALLOTS CAST TOTAL`)
data_17$`REGISTERED VOTERS TOTAL` <- as.numeric(data_17$`REGISTERED VOTERS TOTAL`)

data_16$`BALLOTS CAST TOTAL` <- as.numeric(data_16$`BALLOTS CAST TOTAL`)
data_16$`REGISTERED VOTERS TOTAL` <- as.numeric(data_16$`REGISTERED VOTERS TOTAL`)

data_15$`BALLOTS CAST TOTAL` <- as.numeric(data_15$`BALLOTS CAST TOTAL`)
data_15$`REGISTERED VOTERS TOTAL` <- as.numeric(data_15$`REGISTERED VOTERS TOTAL`)

data_14$`BALLOTS CAST` <- as.numeric(data_14$`BALLOTS CAST`)
data_14$`REG. VOTERS TOTAL` <- as.numeric(data_14$`REG. VOTERS TOTAL`)

data_13$`BALLOTS CAST TOTAL` <- as.numeric(data_13$`BALLOTS CAST TOTAL`)
data_13$`REGISTERED VOTERS TOTAL` <- as.numeric(data_13$`REGISTERED VOTERS TOTAL`)

data_19$`Josh Berkowitz` <- as.numeric(data_19$`Josh Berkowitz`)
data_19$`John Kennedy` <- as.numeric(data_19$`John Kennedy`)

data_23$`Curt           Kissinger` <- as.numeric(data_23$`Curt           Kissinger`)
data_23$`Samantha Silverstein` <- as.numeric(data_23$`Samantha Silverstein`)

data_17$`Curt Kissinger` <- as.numeric(data_17$`Curt Kissinger`)
data_17$`Darlene Rogers` <- as.numeric(data_17$`Darlene Rogers`)

data_15$`Josh Berkowitz` <- as.numeric(data_15$`Josh Berkowitz`)
data_15$`Bob Kelly` <- as.numeric(data_15$`Bob Kelly`)

#### Turnout ####

data_18 <- data_18 %>% 
  mutate(Turnout = `BALLOTS CAST TOTAL`/`REGISTERED VOTERS TOTAL`)

data_19 <- data_19 %>% 
  mutate(Turnout = `BALLOTS CAST TOTAL`/`REGISTERED VOTERS TOTAL`)

data_20 <- data_20 %>% 
  mutate(Turnout = `BALLOTS CAST TOTAL`/`REGISTERED VOTERS TOTAL`)

data_21 <- data_21 %>% 
  mutate(Turnout = `BALLOTS CAST TOTAL`/`REGISTERED VOTERS TOTAL`)

data_22 <- data_22 %>% 
  mutate(Turnout = `BALLOTS CAST TOTAL`/`REGISTERED VOTERS TOTAL`)

data_23 <- data_23 %>% 
  mutate(Turnout = `BALLOTS CAST TOTAL`/`REGISTERED VOTERS TOTAL`)

data_24 <- data_24 %>% 
  mutate(Turnout = `BALLOTS CAST TOTAL`/`REGISTERED VOTERS TOTAL`)

data_17 <- data_17 %>% 
  mutate(Turnout = `BALLOTS CAST TOTAL`/`REGISTERED VOTERS TOTAL`)

data_16 <- data_16 %>% 
  mutate(Turnout = `BALLOTS CAST TOTAL`/`REGISTERED VOTERS TOTAL`)

data_15 <- data_15 %>% 
  mutate(Turnout = `BALLOTS CAST TOTAL`/`REGISTERED VOTERS TOTAL`)

data_14 <- data_14 %>% 
  mutate(Turnout = `BALLOTS CAST`/`REG. VOTERS TOTAL`)

data_13 <- data_13 %>% 
  mutate(Turnout = `BALLOTS CAST TOTAL`/`REGISTERED VOTERS TOTAL`)




turnout <- function(year){
  
  df <- get(paste0("data_", year))
  
  if(year == 14){
    
    registered <- sum(df$`REG. VOTERS TOTAL`, na.rm = TRUE)
    votes <- sum(df$`BALLOTS CAST`, na.rm = TRUE)
    
    turnout <- votes / registered
    
    return(turnout)
    
  }
  
  registered <- sum(df$`REGISTERED VOTERS TOTAL`, na.rm = TRUE)
  votes <- sum(df$`BALLOTS CAST TOTAL`, na.rm = TRUE)
  
  turnout <- votes / registered
  
  return(turnout)
  
}

turn_18 <- turnout(18)
turn_19 <- turnout(19)
turn_20 <- turnout(20)
turn_21 <- turnout(21)
turn_22 <- turnout(22)
turn_23 <- turnout(23)
turn_24 <- turnout(24)
turn_17 <- turnout(17)
turn_16 <- turnout(16)
turn_15 <- turnout(15)
turn_14 <- turnout(14)
turn_13 <- turnout(13)

reg <- sum(data_25$`Registered Voters`, na.rm = TRUE)
  
cast <- sum(data_25$`Ballots Cast`, na.rm = TRUE)

turn_25 <- cast/reg


# Our Prediction
turn_25_pre <- .41

#### Turnout Plot ####

turnout_df <- data.frame(
  year = 2013:2025,
  turnout = c(
    turn_13, turn_14, turn_15, turn_16, turn_17,
    turn_18, turn_19, turn_20, turn_21,
    turn_22, turn_23, turn_24, turn_25
  )
)

turnout_df$election_type <- c(
  "Off-cycle",
  "Sen/Gov",
  "Off-cycle",
  "Presidential",
  "Off-cycle",
  "Sen/Gov",
  "Off-cycle",
  "Presidential",
  "Off-cycle",
  "Sen/Gov",
  "Off-cycle",
  "Presidential",
  "Off-cycle"
)

turnout_df$turnout_pred <- c(
  rep(NA, 12),
  turn_25_pre
)

ggplot(turnout_df, aes(x = year, y = turnout, color = election_type, group = election_type)) +
  geom_line(size = 1.3, na.rm = TRUE) +
  geom_point(size = 3.5, stroke = 1, na.rm = TRUE) +
  geom_text(
    aes(label = scales::percent(turnout, accuracy = 1)),
    vjust = 2.0,
    color = "gray20",
    size = 4,
    show.legend = FALSE,
    na.rm = TRUE
  ) +
  geom_point(
    data = subset(turnout_df, year == 2025),
    aes(x = year, y = turnout_pred),
    color = "#F28E2B", fill = "#F28E2B",
    shape = 21, size = 5, stroke = 1.2,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = subset(turnout_df, year == 2025),
    aes(x = year, y = turnout_pred, label = scales::percent(turnout_pred, accuracy = 1)),
    vjust = -1.2,
    color = "gray20",
    size = 4,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  geom_smooth(
    data = subset(turnout_df, election_type == "Off-cycle"),
    method = "lm", se = FALSE, color = "gray30", linetype = "dashed",
    fullrange = TRUE
  ) +
  scale_x_continuous(
    breaks = turnout_df$year,
    labels = turnout_df$year,
    expand = expansion(mult = c(0.03, 0.12))
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(.25, .9)
  ) +
  scale_color_manual(
    values = c(
      "Presidential" = "#1F77B4",
      "Sen/Gov" = "#59A14F",
      "Off-cycle" = "#B07AA1"
    ),
    na.translate = FALSE
  ) +
  labs(
    title = "Voter Turnout by Election Type (2013–2025)",
    subtitle = "Presidential, Senate/Governor, and Off-cycle elections with 2025 prediction vs actual",
    x = "Year",
    y = "Turnout (%)",
    color = "Election Type"
  ) +
  theme_minimal(base_size = 15, base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 13, color = "gray40", hjust = 0.5),
    axis.title = element_text(face = "bold", size = 13),
    axis.text = element_text(color = "gray25"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.margin = margin(10, 20, 10, 10)
  )

#### Drop-Off ####

data_19 <- data_19 %>% 
  mutate(Cast = `Josh Berkowitz`+`John Kennedy`)

data_23 <- data_23 %>% 
  mutate(Cast = `Curt           Kissinger`+`Samantha Silverstein`)

data_17 <- data_17 %>% 
  mutate(Cast = `Curt Kissinger`+`Darlene Rogers`)

data_15 <- data_15 %>% 
  mutate(Cast = `Josh Berkowitz`+`Bob Kelly`)

data_19 <- data_19 %>% 
  mutate(Drop = 1-(`Cast`/`BALLOTS CAST TOTAL`))

data_23 <- data_23 %>% 
  mutate(Drop = 1-(`Cast`/`BALLOTS CAST TOTAL`))

data_17 <- data_17 %>% 
  mutate(Drop = 1-(`Cast`/`BALLOTS CAST TOTAL`))

data_15 <- data_15 %>% 
  mutate(Drop = 1-(`Cast`/`BALLOTS CAST TOTAL`))

sum <- sum(data_19$`Josh Berkowitz`)

sum_2 <- sum(data_15$`Josh Berkowitz`)

drop <- function(year){
  
  df <- get(paste0("data_", year))
  
  total <- sum(df$`BALLOTS CAST TOTAL`, na.rm = TRUE)
  judge <- sum(df$`Cast`, na.rm = TRUE)
  
  drop <- 1-(judge / total)
  
  return(drop)
  
}

drop_15 <- drop(15)
drop_17 <- drop(17)
drop_19 <- drop(19)
drop_23 <- drop(23)

drop_prediction <- .16
drop_25 <- 1-(37035/39270)

observed_df <- data.frame(
  year = c(2015, 2017, 2019, 2023, 2025),
  turnout = c(drop_15, drop_17, drop_19, drop_23, drop_25)
)

prediction_df <- data.frame(
  year = 2025,
  turnout = drop_prediction
)

ggplot(observed_df, aes(x = year, y = turnout)) +
  annotate("rect", xmin = 2023.2, xmax = 2025.8,
           ymin = -Inf, ymax = Inf, alpha = 0.08, fill = "#d1e6fa") +
  geom_line(size = 1.3, color = "#2c6eaa", alpha = 0.9) +
  geom_point(size = 4, color = "#2c6eaa", stroke = 1.2, fill = "white", shape = 21) +
  geom_text(
    aes(label = percent(turnout, accuracy = 1)),
    vjust = -1, size = 4.2, color = "#2c2c2c"
  ) +
  geom_point(
    data = prediction_df,
    aes(x = year, y = turnout),
    size = 5, shape = 21, fill = "#f0a500", color = "#a97400", stroke = 1.3
  ) +
  geom_text(
    data = prediction_df,
    aes(x = year, y = turnout, label = percent(turnout, accuracy = 1)),
    vjust = -1, size = 4.2, color = "#2c2c2c"
  ) +
  geom_text(
    data = prediction_df,
    aes(x = year, y = turnout, label = "Predicted"),
    hjust = 1.2, vjust = 0.5, fontface = "bold",
    color = "#a97400", size = 4.2
  ) +
  geom_smooth(method = "lm", se = FALSE, color = "#4b4b4b", linetype = "longdash", linewidth = 0.9) +
  scale_x_continuous(
    breaks = c(2015, 2017, 2019, 2023, 2025),
    labels = c(2015, 2017, 2019, 2023, 2025)
  ) +
  scale_y_continuous(
    limits = c(0.05, 0.3),
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "Drop-Off by Year",
    subtitle = "Observed and Predicted Voter Turnout Drop-Off Rates",
    x = "Year",
    y = "Drop-Off (%)",
    caption = "Shaded region indicates projected year"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#1a1a1a", hjust = 0.5),
    plot.subtitle = element_text(color = "#555555", hjust = 0.5, size = 13),
    plot.caption = element_text(color = "#666666", size = 10, hjust = 1),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "#333333"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    plot.background = element_rect(fill = "#f9f9f9", color = NA),
    panel.background = element_rect(fill = "#f9f9f9", color = NA)
  )

#### Magic Number ####

# So drop-off in 2019 was 12.9% and drop-off was 15.4% in 2023.

# So we can predict about a 14% drop-off this year.

# With this we can calculate the magic number for Judge Berkowitz

registered <- 100105

# People that will show up

vote <- registered*turn_25_pre

# People that will vote in our race

judge_voters <- as.integer(vote*(1-drop_prediction))

# Now we get half of the voters in the judicial race and add 1

magic_number <- (judge_voters/2)+1

magic_number

# We predict that Judge Berkowitz will need 15,978 votes to win this election

sum
sum_2

# This is the number of votes he got in his victory in 2019


#### Maps ####

acs_data_total_pop <- acs_interp_judicial %>%
  ggplot(aes(fill = median_ageE)) +
  geom_sf(color = "white", size = 0.2) +  # thin borders for clarity
  scale_fill_viridis_c(
    option = "turbo",
    direction = -1,      # optional: reverse colors so higher = darker
    name = "Median Age"
  ) +
  labs(
    title = "Median Age by Precinct",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    panel.grid.major = element_line(color = "transparent"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    plot.caption = element_text(size = 10, color = "gray50")
  )


acs_data_total_pop


#### Early-Voting Line Graph ####

early25 <- 19425/184265

early24 <- 95856/416548
  
early23 <- 34629/288751
  
early22 <- 32217/307380

early21 <- 10841/156674
  
  
in_person_df <- data.frame(
  year = 2025:2021,
  turnout = c(
    early25, early24, early23, early22, early21
  )
)


ggplot(in_person_df, aes(x = year, y = turnout)) +
  geom_line(linewidth = 1.5, color = "#2C3E50") +
  geom_point(size = 4, color = "#E74C3C", alpha = 0.9) +
  scale_x_continuous(breaks = in_person_df$year) +
  labs(
    title = "Early In-Person Voting Over Time",
    subtitle = "Early Voting Propertion, 2021–2025",
    x = "Year",
    y = "Proportion of Votes Cast Early"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 20, face = "bold", color = "#2C3E50"),
    plot.subtitle = element_text(size = 14, margin = margin(b = 15)),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "#2C3E50"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "#bdc3c7", fill = NA)
  )


  
#### Republican Stuff ####
  
  rep_early_25 <- 2935/19425
  rep_real_25 <- 14579/67296
  
  rep_early_24 <- 4175/32217
  rep_real_24 <- 52103/408361

  rep_early_23 <- 3466/34629
  rep_real_23 <- 49443/276429

  rep_early_22 <- 13521/95856
  rep_real_22 <- 50405/286352
  
  rep_early_21 <- 1616/10841
  rep_real_21 <- 17919/52460


  cool_df <- data.frame(
    year = 2025:2021,
    early = c(
      rep_early_25, rep_early_24, rep_early_23, rep_early_22, rep_early_21
    ),
    total = c(rep_real_25,rep_real_24,rep_real_23,rep_real_22,rep_real_21
    )
  )

  
  cool_df %>%
    pivot_longer(cols = c(early, total), names_to = "type", values_to = "value") %>%
    ggplot(aes(x = factor(year), y = value, fill = type)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65) +
    scale_fill_manual(values = c("early" = "#1f77b4", "total" = "#ff7f0e"),
                      labels = c("Early GOP %", "Total GOP %")) +
    scale_y_continuous(expand = expansion(mult = c(0, .05))) +
    labs(
      x = "Year",
      y = "Proportion",
      fill = ""
    ) +
    theme_minimal(base_size = 18) +
    theme(
      legend.position = "top",
      legend.text = element_text(face = "bold"),
      axis.text = element_text(size = 14),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
      plot.subtitle = element_text(size = 16, hjust = 0.5)
    ) +
    labs(
      title = "Republican Share of Early vs Total Votes",
      subtitle = "Comparing percentages across election years"
    )










  

  
  
