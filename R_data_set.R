library(readxl)
library(tidycensus)
library(tidyverse)
library(dplyr)
library(sf)

# collected data
acs_data_raw <- get_acs(
  geography = "block group",
  variables = c(
    pop_total = "B01003_001",
    white     = "B02001_002",
    black     = "B02001_003",
    asian     = "B02001_005",
    hispanic  = "B03003_003",
    median_age = "B01002_001",
    med_income = "B19013_001",
    
    # Age distribution
    # Male
    m_under5 = "B01001_003", m_5_9 = "B01001_004",
    m_10_14 = "B01001_005", m_15_17 = "B01001_006", m_18_19 = "B01001_007",
    m_20 = "B01001_008", m_21 = "B01001_009", m_22_24 = "B01001_010",
    m_25_29 = "B01001_011", m_30_34 = "B01001_012", m_35_39 = "B01001_013",
    m_40_44 = "B01001_014", m_45_49 = "B01001_015", m_50_54 = "B01001_016",
    m_55_59 = "B01001_017", m_60_61 = "B01001_018", m_62_64 = "B01001_019",
    m_65_66 = "B01001_020", m_67_69 = "B01001_021", m_70_74 = "B01001_022",
    m_75_79 = "B01001_023", m_80_84 = "B01001_024", m_85plus = "B01001_025",
    # Female
    f_under5 = "B01001_027", f_5_9 = "B01001_028",
    f_10_14 = "B01001_029", f_15_17 = "B01001_030", f_18_19 = "B01001_031",
    f_20 = "B01001_032", f_21 = "B01001_033", f_22_24 = "B01001_034",
    f_25_29 = "B01001_035", f_30_34 = "B01001_036", f_35_39 = "B01001_037",
    f_40_44 = "B01001_038", f_45_49 = "B01001_039", f_50_54 = "B01001_040",
    f_55_59 = "B01001_041", f_60_61 = "B01001_042", f_62_64 = "B01001_043",
    f_65_66 = "B01001_044", f_67_69 = "B01001_045", f_70_74 = "B01001_046",
    f_75_79 = "B01001_047", f_80_84 = "B01001_048", f_85plus = "B01001_049"
  ),
  state = "OH",
  county = "Hamilton",
  year = 2023,
  geometry = TRUE,
  output = "wide"
)

income_vars <- c(
  inc_lt10   = "B19001_002",
  inc_10_14  = "B19001_003",
  inc_15_19  = "B19001_004",
  inc_20_24  = "B19001_005",
  inc_25_29  = "B19001_006",
  inc_30_34  = "B19001_007",
  inc_35_39  = "B19001_008",
  inc_40_44  = "B19001_009",
  inc_45_49  = "B19001_010",
  inc_50_59  = "B19001_011",
  inc_60_74  = "B19001_012",
  inc_75_99  = "B19001_013",
  inc_100_124 = "B19001_014",
  inc_125_149 = "B19001_015",
  inc_150_199 = "B19001_016",
  inc_200_plus = "B19001_017"
)

acs_income <- get_acs(
  geography = "block group",
  variables = income_vars,
  state = "OH",
  county = "Hamilton",
  year = 2023,
  geometry = FALSE,  # no need to pull geometry again
  output = "wide"
)

# ---- Merge income detail into base ----
acs_merged <- left_join(acs_data_raw, acs_income, by = "GEOID")

acs_merged <- acs_merged %>%
  mutate(NAME = paste0("Block Group ", substr(GEOID, 12, 12), 
                       ", Tract ", substr(GEOID, 6, 11),
                       ", Hamilton County, OH"))

# Collapse into combined sex + 10-year bins
acs_merged <- acs_merged %>%
  mutate(
    age_0_9   = m_under5E + m_5_9E + f_under5E + f_5_9E,
    age_10_19 = m_10_14E + m_15_17E + m_18_19E + f_10_14E + f_15_17E + f_18_19E,
    age_20_29 = m_20E + m_21E + m_22_24E + m_25_29E +
      f_20E + f_21E + f_22_24E + f_25_29E,
    age_30_39 = m_30_34E + m_35_39E + f_30_34E + f_35_39E,
    age_40_49 = m_40_44E + m_45_49E + f_40_44E + f_45_49E,
    age_50_59 = m_50_54E + m_55_59E + f_50_54E + f_55_59E,
    age_60_69 = m_60_61E + m_62_64E + m_65_66E + m_67_69E +
      f_60_61E + f_62_64E + f_65_66E + f_67_69E,
    age_70_79 = m_70_74E + m_75_79E + f_70_74E + f_75_79E,
    age_80plus = m_80_84E + m_85plusE + f_80_84E + f_85plusE
  )

# ---- Collapse INCOME into cohorts (example bins) ----
acs_final <- acs_merged %>%
  mutate(
    hhinc_under25k = inc_lt10E + inc_10_14E + inc_15_19E + inc_20_24E,
    hhinc_25_49k   = inc_25_29E + inc_30_34E + inc_35_39E + inc_40_44E + inc_45_49E,
    hhinc_50_99k   = inc_50_59E + inc_60_74E + inc_75_99E,
    hhinc_100_149k = inc_100_124E + inc_125_149E,
    hhinc_150_199k = inc_150_199E,
    hhinc_200plus  = inc_200_plusE
  ) %>%
  select(
    GEOID, NAME, geometry,
    pop_totalE, whiteE, blackE, asianE, hispanicE,
    median_ageE, med_incomeE,
    starts_with("age_"), starts_with("hhinc_")
  )

acs_final$med_incomeE[is.na(acs_final$med_incomeE)] <-
  (12500  * acs_final$hhinc_under25k[is.na(acs_final$med_incomeE)] +
     37500  * acs_final$hhinc_25_49k[is.na(acs_final$med_incomeE)] +
     75000  * acs_final$hhinc_50_99k[is.na(acs_final$med_incomeE)] +
     125000  * acs_final$hhinc_100_149k[is.na(acs_final$med_incomeE)] +
     175000  * acs_final$hhinc_150_199k[is.na(acs_final$med_incomeE)] +
     250000  * acs_final$hhinc_200plus[is.na(acs_final$med_incomeE)]) /
  (acs_final$hhinc_under25k[is.na(acs_final$med_incomeE)] +
     acs_final$hhinc_25_49k[is.na(acs_final$med_incomeE)] +
     acs_final$hhinc_50_99k[is.na(acs_final$med_incomeE)] +
     acs_final$hhinc_100_149k[is.na(acs_final$med_incomeE)] +
     acs_final$hhinc_150_199k[is.na(acs_final$med_incomeE)] +
     acs_final$hhinc_200plus[is.na(acs_final$med_incomeE)])

acs_final$other_race <- pmax(0, acs_final$pop_totalE - (acs_final$whiteE + 
                                                          acs_final$blackE + 
                                                          acs_final$asianE + 
                                                          acs_final$hispanicE))


acs_extensive <- acs_final %>%
  select(GEOID, geometry,
         pop_totalE, whiteE, blackE, asianE, hispanicE, other_race,
         starts_with("age_"), starts_with("hhinc_")) %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON", warn = FALSE)

acs_intensive <- acs_final %>%
  select(GEOID, geometry, median_ageE, med_incomeE) %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON", warn = FALSE)

# Judicial Interpolation

judicial_boundaries <- st_read("shapefiles/judicial_boundary.shp") %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON", warn = FALSE)
judicial_precincts  <- st_read("shapefiles/judicial_precincts.shp")%>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON", warn = FALSE)

block.total <-get_decennial(geography = "block",
                            state = "Ohio",
                            county = "Hamilton",
                            variables = "P1_001N",
                            year = 2020,
                            sumfile = "dhc",
                            geometry = TRUE) %>%
  select(pop_totalE = value)

acs_interp_j_ext <- interpolate_pw(
  from        = st_make_valid(acs_extensive),
  to          = st_make_valid(judicial_precincts),
  to_id = "PRECINCT",
  extensive   = TRUE,
  weights     = st_make_valid(block.total),
  weight_column = "pop_totalE",
  crs         = 4269
)

acs_interp_j_int <- interpolate_pw(
  from          = st_make_valid(acs_intensive),
  to            = st_make_valid(judicial_precincts),
  to_id = "PRECINCT",
  extensive     = FALSE,
  weights       = st_make_valid(block.total),
  weight_column = "pop_totalE",
  crs           = 4269
) %>%
  st_drop_geometry()

acs_interp_judicial <- acs_interp_j_ext %>%
  left_join(acs_interp_j_int, by = "PRECINCT")

# cps interpolation

cps_boundaries <- st_read("shapefiles/cps_boundary.shp")
cps_precincts <- st_read("shapefiles/cps_precincts.shp")

acs_interp_cps_ext <- interpolate_pw(
  from        = st_make_valid(acs_extensive),
  to          = st_make_valid(cps_precincts),
  to_id = "PRECINCT",
  extensive   = TRUE,
  weights     = st_make_valid(block.total),
  weight_column = "pop_totalE",
  crs         = 4269
)


acs_interp_cps_int <- interpolate_pw(
  from          = st_make_valid(acs_intensive),
  to            = st_make_valid(cps_precincts),
  to_id = "PRECINCT",
  extensive     = FALSE,
  weights       = st_make_valid(block.total),
  weight_column = "pop_totalE",
  crs           = 4269
) %>%
  st_drop_geometry()

acs_interp_cps <- acs_interp_cps_ext %>%
  left_join(acs_interp_cps_int, by = "PRECINCT")

# cincy interpolation
cincy_boundaries <- st_read("shapefiles/cincy_boundary.shp")
cincy_precincts <- st_read("shapefiles/cincy_precincts.shp")

acs_interp_cincy_ext <- interpolate_pw(
  from        = st_make_valid(acs_extensive),
  to          = st_make_valid(cincy_precincts),
  to_id = "PRECINCT",
  extensive   = TRUE,
  weights     = st_make_valid(block.total),
  weight_column = "pop_totalE",
  crs         = 4269
)


acs_interp_cincy_int <- interpolate_pw(
  from          = st_make_valid(acs_intensive),
  to            = st_make_valid(cincy_precincts),
  to_id = "PRECINCT",
  extensive     = FALSE,
  weights       = st_make_valid(block.total),
  weight_column = "pop_totalE",
  crs           = 4269
) %>%
  st_drop_geometry()

acs_interp_cincy <- acs_interp_cincy_ext %>%
  left_join(acs_interp_cincy_int, by = "PRECINCT")

#hamilton county interpolation

ham_boundaries <- st_read("shapefiles/cincy_boundary.shp")
ham_precincts <- st_read("shapefiles/precincts_2024.shp")

acs_interp_ham_ext <- interpolate_pw(
  from        = st_make_valid(acs_extensive),
  to          = st_make_valid(ham_precincts),
  to_id = "PRC_NAME",
  extensive   = TRUE,
  weights     = st_make_valid(block.total),
  weight_column = "pop_totalE",
  crs         = 4269
)


acs_interp_ham_int <- interpolate_pw(
  from          = st_make_valid(acs_intensive),
  to            = st_make_valid(ham_precincts),
  to_id = "PRC_NAME",
  extensive     = FALSE,
  weights       = st_make_valid(block.total),
  weight_column = "pop_totalE",
  crs           = 4269
) %>%
  st_drop_geometry()


acs_interp_ham <- acs_interp_ham_ext %>%
  left_join(acs_interp_ham_int, by = "PRC_NAME")

acs_interp_ham <- acs_interp_ham %>%
  rename(PRECINCT = PRC_NAME)

save(acs_interp_judicial, acs_interp_cps, acs_interp_cincy, acs_interp_ham, file = "acs_data.RData")

