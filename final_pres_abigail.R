library(tidyverse)
library(sf)
library(readxl)
library(RColorBrewer)
#### BASE?SWING ####

precincts <- st_read("shapefiles/2025 Shape Files.shp")
boundary  <- st_read("shapefiles/judicial_boundary.shp")

df <- read_excel("data/election results/2025detail.xlsx", sheet = 45)
colnames(df) <- c("Precinct", "Registered_voters", "election_dc", "early_dc", "total_dc", "election_jb", "early_jb", "total_jb", "total")
df <- df[-c(1, 2), ]

df <- df %>%
  mutate(
    total_dem = as.numeric(total_dc),
    total_rep = as.numeric(total_jb),
    vote_total = total_dem + total_rep,
    pct_dem = total_dem / vote_total,
    pct_rep = total_rep / vote_total,
    swing = pct_dem - pct_rep,        # original: positive = D, negative = R
    swing_flipped = -swing,           # flip: negative = D, positive = R
    swing_scaled = (swing_flipped + 1) / 2  # scale to 0-1 for color
  )


abbr_lookup <- c(
  "CIN"  = "CINCINNATI",
  "NORW" = "NORWOOD",
  "AND"  = "ANDERSON",
  "NEWT" = "NEWTOWN",
  "FAIRFX" = "FAIRFAX",
  "COLUM" = "COLUMBIA",
  "MARMT" = "MARIEMONT"
)

# Parse df$Precinct like "0101 CIN 1-A"
df <- df %>%
  mutate(
    Precinct = str_trim(as.character(Precinct)),
    # capture groups: leading digits (ignored), abbrev, precinct id (rest)
    parsed = str_match(Precinct, "^\\s*\\d+\\s+([A-Z]+)\\s+(.+)$"),
    abbr = parsed[,2],
    precinct_id = parsed[,3],
    # If some rows don't match the pattern, try a looser pattern:
    parsed2 = if_else(is.na(abbr),
                      str_match(Precinct, "^\\s*([A-Z]+)\\s+(.+)$"),
                      parsed),
    abbr = if_else(is.na(abbr), parsed2[,2], abbr),
    precinct_id = if_else(is.na(precinct_id), parsed2[,3], precinct_id),
    County_full = abbr_lookup[abbr],
    County_full = if_else(is.na(County_full), toupper(abbr), County_full),  # fallback
    precinct_id = str_trim(precinct_id),
    County_full = str_trim(toupper(County_full)),
    # build df join key matching "PRC_NAME - PRECINCT" format
    join_key = paste0(County_full, " ", precinct_id)
  ) %>%
  select(-parsed, -parsed2)   # drop helper cols if you like

# Prepare precincts shapefile join_key: ensure PRC_NAME & PRECINCT are formatted
precincts <- precincts %>%
  mutate(
    PRC_NAME = toupper(str_trim(as.character(PRC_NAME))),
    PRECINCT = str_trim(as.character(PRECINCT)),
    join_key = paste0(PRC_NAME)
  )

precincts_filtered <- precincts %>%
  semi_join(df, by = "join_key")

precincts_swing <- precincts_filtered %>%
  left_join(df, by = "join_key") %>%
  st_as_sf()

ggplot(precincts_swing) +
  geom_sf(aes(fill = swing_scaled), color = "black", size = 0.2) +
  geom_sf(data = boundary, fill = NA, color = "black", size = 0.5) +
  scale_fill_gradientn(
    colours = rev(brewer.pal(n = 10, name = "RdBu")),
    na.value = "transparent",
    values = c(0, 0.5, 1),
    breaks = c(0, 0.5, 1),
    labels = c("100% Democract", "Even", "100% Republican"),
    limits = c(0, 1)
  ) +
  labs(
    title = "2025 Swing Map Judicial Election District 4",
    subtitle = "Democratic vs Republican Vote Swing",
    fill = "Support Level"
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank()
  )

#### 2023 people ####
data <- read.csv("data/HAMILTON (1).txt")
precinct_counts <- data %>%
  filter(
    GENERAL.11.07.2023 == "X" &
    GENERAL.11.04.2025 == "X" &
    GENERAL.11.02.2021 != "X" &
    GENERAL.11.05.2019 != "X"
  ) %>%
  group_by(PRECINCT_NAME) %>%
  summarise(
    count = n(),
    .groups = "drop"
  )


precinct_pct <- precinct_counts %>%
  left_join(
    df %>% select(join_key, total),
    by = c("PRECINCT_NAME" = "join_key")
  ) %>%
  mutate(
    total = as.numeric(total),
    pct = count / total
  )
precinct_pct <- st_drop_geometry(precinct_pct)

precinct_pct <- precincts %>%
  semi_join(df, by = c("join_key" = "join_key")) %>%
  left_join(precinct_pct, by = c("join_key" = "PRECINCT_NAME"))

ggplot(precinct_pct) +
  geom_sf(aes(fill = pct), color = "black", size = 0.2) +
  geom_sf(data = boundary, fill = NA, color = "black", size = 0.5) +
  scale_fill_gradientn(
    colours = brewer.pal(9, "BuPu"),
    labels = scales::percent_format(accuracy = 1),
    na.value = "transparent"
  ) +
  labs(
    title = "Percent of Voters Who Voted in 2023 & 2025 but Not 2019 or 2021",
    subtitle = "Share of precinct electorate",
    fill = "% of voters"
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank()
  )

#### tunout map ####
df <- df %>%
  mutate(turnout = as.numeric(total) / as.numeric(Registered_voters))

precincts_turnout <- precincts_filtered %>%
  left_join(df, by = "join_key") %>%
  st_as_sf()

ggplot(precincts_turnout) +
  geom_sf(aes(fill = turnout), color = "black", size = 0.2) +
  geom_sf(data = boundary, fill = NA, color = "black", size = 0.5) +
  scale_fill_gradientn(
    colours = brewer.pal(9, "PiYG"),
    labels = scales::percent_format(accuracy = 1),
    na.value = "transparent"
  ) +
  labs(
    title = "Turnout map by Precinct",
    fill = "Turnout"
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank()
  )


precincts_swing <- st_drop_geometry(precincts_swing)

comparison <- combined_w %>%
  left_join(
    precincts_swing %>% select(PRC_NAME, swing_scaled),
    by = "PRC_NAME"
  )

comparison <- comparison %>%
  mutate(residual = weighted_rep - swing_scaled)

#### calculations ####

sd_residuals <- sd(comparison$residual, na.rm = TRUE)
sd_residuals

r2 <- cor(comparison$weighted_rep, comparison$swing_scaled, use = "complete.obs")^2
r2

df$Registered_voters <- as.numeric(gsub(",", "", df$Registered_voters))
df$total <- as.numeric(gsub(",", "", df$total))

total_registered <- tail(df$Registered_voters, 1)
total_voted <- tail(df$total, 1)

turnout <- total_voted / total_registered
turnout
