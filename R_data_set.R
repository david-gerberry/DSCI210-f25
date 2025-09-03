library(tidycensus)
library(tidyverse)
library(dplyr)

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

# Collapse into combined sex + 10-year bins
acs_data <- acs_data_raw %>%
  mutate(
    age_0_9   = m_under5E + m_5_9E + f_under5E + f_5_9E,
    age_10_19 = m_10_14E + m_15_17E + m_18_19E +
      f_10_14E + f_15_17E + f_18_19E,
    age_20_29 = m_20E + m_21E + m_22_24E + m_25_29E +
      f_20E + f_21E + f_22_24E + f_25_29E,
    age_30_39 = m_30_34E + m_35_39E + f_30_34E + f_35_39E,
    age_40_49 = m_40_44E + m_45_49E + f_40_44E + f_45_49E,
    age_50_59 = m_50_54E + m_55_59E + f_50_54E + f_55_59E,
    age_60_69 = m_60_61E + m_62_64E + m_65_66E + m_67_69E +
      f_60_61E + f_62_64E + f_65_66E + f_67_69E,
    age_70_79 = m_70_74E + m_75_79E + f_70_74E + f_75_79E,
    age_80plus = m_80_84E + m_85plusE + f_80_84E + f_85plusE
  ) %>%
  select(GEOID, NAME, geometry, pop_totalE, whiteE, blackE, asianE,
         hispanicE, median_ageE, med_incomeE,
         starts_with("age_"))

write_csv("Acs_Data.csv", acs_data )
