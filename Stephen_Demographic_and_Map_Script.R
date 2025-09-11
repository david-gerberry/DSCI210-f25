library(tidyverse)
library(tidycensus)
library(ggplot2)
library(sf)
library(dplyr)



#### voter data ####

voter_df <- read_csv("data/VoterListExport-20250911-CICINC-CI-pn.csv")

voter_df_clean <- voter_df %>% select(-5:-31)
  
write.csv(voter_df_clean, "data/voter_df_clean.csv")

voter_df <- read_csv("data/voter_df_clean.csv")


count_a_u <- "A-U"

voter_df$num_absnt_un <- apply(voter_df[, 6:ncol(voter_df)], 1, function(row) {
  sum(row == count_a_u, na.rm = TRUE)
})

count_p_u <- "P-U"

voter_df$num_polls_un <- apply(voter_df[, 6:ncol(voter_df)], 1, function(row) {
  sum(row == count_p_u, na.rm = TRUE)
})

count_p_d <- "P-D"

voter_df$num_polls_d <- apply(voter_df[, 6:ncol(voter_df)], 1, function(row) {
  sum(row == count_p_d, na.rm = TRUE)
})

count_p_r <- "P-R"

voter_df$num_polls_r <- apply(voter_df[, 6:ncol(voter_df)], 1, function(row) {
  sum(row == count_p_r, na.rm = TRUE)
})

count_a_d <- "A-D"

voter_df$num_absnt_d <- apply(voter_df[, 6:ncol(voter_df)], 1, function(row) {
  sum(row == count_a_d, na.rm = TRUE)
})

count_a_r <- "A-R"

voter_df$num_absnt_r <- apply(voter_df[, 6:ncol(voter_df)], 1, function(row) {
  sum(row == count_a_r, na.rm = TRUE)
})


stuff_by_presinct <-voter_df %>%
  group_by(PrecinctNum)%>%
  summarise(sum_polls_un = sum(num_polls_un, na.rm = TRUE),
            sum_polls_d = sum(num_polls_d, na.rm = TRUE),
            sum_polls_r = sum(num_polls_r, na.rm = TRUE),
            sum_absnt_d = sum(num_absnt_d, na.rm = TRUE),
            sum_absnt_r = sum(num_absnt_r, na.rm = TRUE),
            sum_absnt_un = sum(num_absnt_un, na.rm = TRUE))

write.csv(stuff_by_presinct, "data/sum_by_precinct.csv")

summarized_data <- read_csv("data/sum_by_precinct.csv")


summarized_data_long <- summarized_data %>%
  pivot_longer(
    cols = starts_with("sum_"),
    names_to = "Category",
    values_to = "Count"
  )


ggplot(summarized_data_long, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  facet_wrap(~PrecinctNum, scales = "free_y") +
  labs(
    title = "Voting Status Frequency by Precinct",
    x = "Voting Category",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

#### Pulling Census Data ####

census_api_key("4951cbe6bdec5269e91df7656ba86ee58a29cff7")

Census_Data <- get_acs(
  geography = "block group",
  variables = c(
    med_household_income = "B19013_001"#,
    #median_age_pop = "B01002_001",
    #median_age_fam_hh = "B11007_002",      
    #median_age_nonfam_hh = "B11007_003" 
  ), 
  state = "OH",
  county= "Hamilton",
  year = 2023,
  geometry = TRUE
)

Census_Data_2 <- get_acs(
  geography = "block group",
  variables = c(
    #med_household_income = "B19013_001"#,
    median_age_pop = "B01002_001",
    median_age_fam_hh = "B11007_002",      
    median_age_nonfam_hh = "B11007_003" 
  ), 
  state = "OH",
  county= "Hamilton",
  year = 2023,
  geometry = TRUE
)


#### Map Making ####



households <- get_acs(geography = "block group", 
                             state = "Ohio",
                             county = "Hamilton",
                             variables = "B11001_001",
                             year = 2023,
                             geometry = TRUE) %>% 
  select(GEOID, households = estimate) 


population <- get_acs(geography = "block group",
                                state = "OH",
                                county = "Hamilton",
                                variables = "B01003_001",   # Total population
                                year = 2023,
                                geometry = TRUE) %>%
  select(GEOID, total.pop = estimate)



cincy_precincts <- st_read("shapefiles/cincy_precincts.shp")
cincy_boundary <- st_read("shapefiles/cincy_boundary.shp")

cincy_boundary <- st_set_crs(cincy_boundary, 4269)
cincy_precincts <- st_set_crs(cincy_precincts, 4269)



#### Interpolation ####


precinct_household_income_interpolated <- interpolate_pw(
  from = st_make_valid(Census_Data),
  to = st_make_valid(cincy_precincts),
  extensive = FALSE,                 ## because you don't want to add the median ages,
  weights =st_make_valid(households),
  weight_column = "households",
  crs = 4269
) %>% rename(med_household_income = estimate)



#Interpolate median ages
precinct_age_fam_interpolated <- interpolate_pw(
  from = st_make_valid(Census_Data),
  to = st_make_valid(cincy_precincts),
  extensive = FALSE,
  weights = st_make_valid(population),     # Use population weights
  weight_column = "total.pop",
  crs = 4269
) %>% rename(median_age_fam_hh = estimate)

precinct_age_nonfam_interpolated <- interpolate_pw(
  from = st_make_valid(Census_Data),
  to = st_make_valid(cincy_precincts),
  extensive = FALSE,
  weights = st_make_valid(population),     # Use population weights
  weight_column = "total.pop",
  crs = 4269
) %>% rename(median_age_nonfam_hh = estimate)


#### Plotting Maps ####


# Family householders this is all fucked
precinct_age_fam_interpolated %>%
  ggplot(aes(fill = median_age_fam_hh)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(option = "magma") +
  labs(fill = "Median Age (Family HH)") +
  theme_minimal()

# Non-family householders this is all fucked 
precinct_age_nonfam_interpolated %>%
  ggplot(aes(fill = median_age_nonfam_hh)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(option = "turbo") +
  labs(fill = "Median Age (Nonfamily HH)") +
  theme_minimal()


#household Income only thing that works
precinct_household_income_interpolated %>%
  ggplot(aes(fill = med_household_income)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(option = "turbo") +
  labs(fill = "Median Household Income ($)") +
  theme_minimal()

