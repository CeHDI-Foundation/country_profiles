# Setup ####
pacman::p_load(
  here,
  tidyverse,
  # plotly,
  janitor,
  DT,
  sf,
  necountries,
  patchwork,
  Hmisc,
  ggmapinset
)

# Load or install packages from GitHub:
pacman::p_load_gh(
  "DrMattG/SDGsR", # Uses API to get SDGs data
  "aphp/rgho" # Uses API to get data from Global Health Observatory
  # "PPgp/wpp2024" # United Nations World Population Prospects 2024
  # "m-muecke/isocountry" # Get ISO codes for countries
)

state_geo <- readRDS(here("data", "state_geo_enhanced.rds"))

# Load in custom functions
source(here("utils.R"))

country_list <- tibble(
  country = state_geo$country,
  english_formal = state_geo$english_formal
)

theme_labels <- tribble(
  ~"variable", ~"theme_label",
  "health_systems", "Health systems and services",
  "emergencies", "Health security, emergencies, and disaster relief",
  "ncd", "Non-communicable diseases",
  "communicable", "Communicable diseases",
  "SRHR", "Sexual and reproductive health and rights",
  "mental_health", "Mental health",
  "SOCED", "Social and economic determinants of health",
  "GBV", "Gender-based violence",
  "women", "Women's health",
  "MCAH", "Maternal, child, and adolescent health",
  "essential_medicines", "Essential medicines and health products",
  "disabilities", "Disabilities and health",
  "LGBTI", "Health of LGBTI persons",
  "HIV", "HIV/AIDS and STIs",
  "TB_malaria", "TB and malaria",
  "NTD", "Neglected tropical diseases",
  "TB_malaria_NTD", "TB, malaria, and neglected tropical diseases",
  "vaccinations", "Vaccinations",
  "WASH", "Water and Sanitation",
  "nutrition", "Nutrition",
  "maternal_health", "Maternal health",
  "abortion", "Abortion",
  "incarcerated", "Health of incarcerated persons"
)


# World abortion laws -----------------------------------------------------
# https://reproductiverights.org/maps/worlds-abortion-laws/

gestational_text <- "W5|W8|W10|W12|W13|W14|W16|W17|W18|W20|W22|W24|D90|D120|†|°|º"
world_abortion_laws <- readxl::read_xlsx(here("data", "world_abortion_laws.xlsx")) |> 
  janitor::clean_names() |> 
  mutate(country = str_remove(country, gestational_text)) |> 
  arrange(country) |> 
  mutate(category = factor(case_match(
    category,
    "I" ~ "I. On Request",
    "II" ~ "II. Socioeconomic Grounds",
    "III" ~ "III. To Preserve Health",
    "IV" ~ "IV. To Save the Mother's Life",
    "V" ~ "V. Prohibited Altogether",
    "Varies" ~ "Varies at State level"
  ))) |> 
  mutate(country = case_match(
    country,
    "Antigua & Barbuda" ~ "Antigua and Barbuda",
    "Bolivia" ~ "Bolivia (Plurinational State of)",
    "Bosnia & Herzegovina" ~ "Bosnia and Herzegovina",
    "Cape Verde" ~ "Cabo Verde",
    "Central African Rep." ~ "Central African Republic",
    "Czech Rep." ~ "Czechia",
    "Dem. People’s Rep. of Korea" ~ "Democratic People's Republic of Korea",
    "Dem. Rep. of Congo" ~ "Democratic Republic of the Congo",
    "Eswatini (formerly Swaziland)" ~ "Eswatini",
    "Guinea Bissau" ~ "Guinea-Bissau",
    "Iran" ~ "Iran (Islamic Republic of)" ,
    "Ivory Coast" ~ "Côte d'Ivoire",
    "Laos" ~ "Lao People's Democratic Republic",
    "Micronesia" ~ "Micronesia (Federated States of)",
    "Moldova" ~ "Republic of Moldova",
    "Dem. People’s Rep. of Korea" ~ "Democratic People's Republic of Korea",
    "Russian Fed." ~ "Russian Federation",
    "Saint Kitts & Nevis" ~ "Saint Kitts and Nevis",
    "Saint Vincent & the Grenadines" ~ "Saint Vincent and the Grenadines",
    "São Tomé & Príncipe" ~ "Sao Tome and Principe",
    "Slovak Rep." ~ "Slovakia",
    "South Korea" ~ "Republic of Korea",
    "Syria" ~ "Syrian Arab Republic",
    "Trinidad & Tobago" ~ "Trinidad and Tobago",
    "Tanzania" ~ "United Republic of Tanzania",
    "Turkey" ~ "Türkiye",
    "Great Britain" ~ "United Kingdom of Great Britain and Northern Ireland",
    "Venezuela" ~ "Venezuela (Bolivarian Republic of)",
    "Vietnam" ~ "Viet Nam",
    .default = country
  ))

# # SDGs data ####
# Goals<-get_SDGs_goals()
# ind_lst<-get_indicator_list()
# 
# ind_lst %>%
#   filter(goal %in% c("3")) |> 
#   select(target, code, description) |> 
#   rename(goal = target, target = code) |> 
#   kableExtra::kable()
# 
# ind_lst %>%
#   filter(target %in% c("5.2", "5.3", "5.6")) |> 
#   select(target, code, description) |> 
#   rename(goal = target, target = code) |> 
#   kableExtra::kable()

# ind_lst %>%
#   filter(code %in% c("3.1.1", "3.1.2", "3.2.1", "3.2.2", "5.6.1")) %>%
#   select(code, description) %>%
#   kableExtra::kable()

# partner_violence <- get_indicator_data("5.2.1")
# sexual_violence <- get_indicator_data("5.2.2")
# health_expenditure <- get_indicator_data("3.8.2")
# mmr_sdg <- get_indicator_data("3.1.1") |>
#   left_join(SDGsR::lookup_country_codes |> select(-m49_code),
#             join_by(geoAreaName == country_or_area)) |>
#   mutate(across(c(value, upperBound, lowerBound), ~ as.numeric(.x)),
#          year = ymd(paste0(timePeriodStart, "-01-01"))
#          )
# 
# mmr_sdg_region <- mmr_sdg |> filter(is.na(iso_alpha3_code))

# --- GHO data ####
# Make API calls to the GHO database to download the data related to the UHC indicators and to the indicator for maternal mortality ratio

# Code for searching in GHO indicators
a<-search_values("pregnancy", dimension = "GHO")

## Metadata ####
gho_codes <- get_gho_values(dimension = "GHO") |> 
  rename(IndicatorCode=Code, indicator_fullname = Title)
country_codes <- get_gho_values(dimension = "COUNTRY") |> 
  rename(COUNTRY = Code, country_name = Title)
region_codes <- get_gho_values(dimension = "REGION") |> 
  rename(REGION = Code, region_name = Title)

POP_2010 <- get_gho_data(code = "RS_1845")

## UHC data ####
UHC_AVAILABILITY_SCORE <- get_gho_data(code = "UHC_AVAILABILITY_SCORE")
UHC_INDEX_REPORTED <- get_gho_data(code = "UHC_INDEX_REPORTED")
UHC_SCI_CAPACITY <- get_gho_data(code = "UHC_SCI_CAPACITY")
UHC_SCI_INFECT <- get_gho_data(code = "UHC_SCI_INFECT")
UHC_SCI_NCD <- get_gho_data(code = "UHC_SCI_NCD")
UHC_SCI_RMNCH <- get_gho_data(code = "UHC_SCI_RMNCH")

# Combine the datasets and tag indicator and country full names
UHC_all <- bind_rows(UHC_AVAILABILITY_SCORE, UHC_INDEX_REPORTED, 
                     UHC_SCI_CAPACITY, UHC_SCI_INFECT,
                     UHC_SCI_NCD, UHC_SCI_RMNCH) |> 
  left_join(country_codes) |> 
  left_join(gho_codes) |> 
  left_join(region_codes) |> 
  mutate(
    NumericValue = as.numeric(NumericValue),
    year = ymd(paste0(YEAR, "-01-01")),
    region_name = case_when(GLOBAL == "GLOBAL" ~ "Global", 
                            .default = region_name)
  )
## Maternal mortality ratio ####
MMR <- get_gho_data(code = "MDG_0000000026") |> 
  left_join(country_codes) |> 
  left_join(gho_codes) |> 
  left_join(region_codes) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01")),
    region_name = case_when(GLOBAL == "GLOBAL" ~ "Global", 
                            .default = region_name)
  ) |> 
  mutate(
    mmr_cat = factor(case_when(
      NumericValue < 10 ~ "<10",
      NumericValue < 20 ~ "10-19",
      NumericValue < 100 ~ "20-99",
      NumericValue < 300 ~ "100-299",
      NumericValue < 500 ~ "300-499",
      NumericValue >= 500 ~ "500+",
      .default = NA), 
      levels = c("<10","10-19", "20-99","100-299", "300-499", "500+" )
    )
  )

## HIV ####
HIV_death <- get_gho_data(code = "HIV_0000000006") |> 
  left_join(country_codes) |> 
  left_join(gho_codes) |> 
  left_join(region_codes) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01")),
    region_name = case_when(GLOBAL == "GLOBAL" ~ "Global", 
                            .default = region_name)
  )

## Family planning ####
family_planning <- get_gho_data(code = "SDGFPALL") |> 
  left_join(country_codes) |> 
  left_join(gho_codes) |> 
  left_join(region_codes) |> 
  mutate(
    NumericValue = as.numeric(NumericValue),
    # across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01")),
    region_name = case_when(GLOBAL == "GLOBAL" ~ "Global", 
                            .default = region_name)
  ) |> 
  mutate(
    value_cat = factor(case_when(
      NumericValue< 10 ~ "<10",
      NumericValue< 30 ~ "<30",
      NumericValue< 60 ~ "<60",
      NumericValue< 90 ~ "<90",
      NumericValue>= 90 ~ "90+",
      .default = NA), 
      levels = c("10", "<30", "<60", "<90", "90+")
    ))

## Skilled birth ####
skilled_birth <- get_gho_data(code = "MDG_0000000025") |> 
  left_join(country_codes) |> 
  left_join(gho_codes) |> 
  left_join(region_codes) |> 
  mutate(
    NumericValue = as.numeric(NumericValue),
    # across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01")),
    region_name = case_when(GLOBAL == "GLOBAL" ~ "Global", 
                            .default = region_name)
  ) |> 
  mutate(
    value_cat = factor(case_when(
      NumericValue< 60 ~ "<60",
      NumericValue< 70 ~ "60-69",
      NumericValue< 80 ~ "70-79",
      NumericValue< 90 ~ "80-89",
      NumericValue< 98 ~ "90-97",
      NumericValue>= 98 ~ "98+",
      .default = NA), 
      levels = c("<60", "60-69", "70-79", "80-89", "90-97", "98+")
    ))

## Adolescent birth ####
adolescent_birth <- get_gho_data(code = "MDG_0000000003") |> 
  left_join(country_codes) |> 
  left_join(gho_codes) |> 
  left_join(region_codes) |> 
  mutate(
    NumericValue = as.numeric(NumericValue),
    # across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01")),
    region_name = case_when(GLOBAL == "GLOBAL" ~ "Global", 
                            .default = region_name)
  )

## Abortion rate ####
abortion_rate <- get_gho_data(code = "SRH_ABORTION_RATE") |> 
  left_join(country_codes) |> 
  left_join(gho_codes) |> 
  # left_join(region_codes) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01")),
    # region_name = case_when(GLOBAL == "GLOBAL" ~ "Global", 
    #                         .default = region_name)
  )

## Abortion rate ####
unintended_pregnancy <- get_gho_data(code = "SRH_PREGNANCY_UNINTENDED_RATE") |> 
  left_join(country_codes) |> 
  left_join(gho_codes) |> 
  # left_join(region_codes) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01")),
    # region_name = case_when(GLOBAL == "GLOBAL" ~ "Global", 
    #                         .default = region_name)
  )

# --- GBD data ####
# Global Burden of Disease Collaborative Network. Global Burden of Disease Study 2021 (GBD 2021) Results.
# Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2022.
# Available from <https://vizhub.healthdata.org/gbd-results/>.

## All causes data ####
# **Query:**  
#   
#   **GBD estimate:** Cause of death of injury  
# **Measure:** Deaths; DALYs  
# **Metric:** Rate  
# **Cause:** All level 3 causes  
# **Location:** All countries and territories  
# **Age:** Age-standardized  
# **Sex:** Both; Female; Male  
# **Year:** Select all  
# 
# This query generated 14 CSV files that I combined and then saved as RDS format in order to save diskspace

# Uncomment below code for first run
# GBD <-
#   # List the filenames of each CSV file in the folder
#   list.files(path = here("data", "GBD"),
#                   pattern = "\\.csv$",
#                   full.names = TRUE) |>
#   # Read them in using data.table::fread()
#   map_df(~data.table::fread(.)) |>
#   # Clean the names
#   janitor::clean_names() |>
#   # Format variables
#   mutate(
#     across(c(measure_name, location_name, sex_name,
#                   cause_name, metric_name), as.factor),
#     date = ymd(paste0(year, "-01-01"))
#     )
# 
# # Split the file by measure
# GBD_DALY <- GBD |> filter(measure_name == "DALYs (Disability-Adjusted Life Years)")
# GBD_deaths <- GBD |> filter(measure_name == "Deaths")
# 
# # Save as RDS
# saveRDS(GBD_DALY, file = here("data", "GBD", "GBD_DALY.rds"))
# saveRDS(GBD_deaths, file = here("data", "GBD", "GBD_deaths.rds"))

GBD_DALY <- readRDS(here("data", "GBD", "GBD_DALY.rds")) |> 
  left_join(country_list, join_by(location_name == english_formal)) |> 
  mutate(country = case_when(location_name == "Global" ~ "Global", 
                             .default = country)) |> 
  filter(!is.na(country))

GBD_deaths <- readRDS(here("data", "GBD", "GBD_deaths.rds"))  |> 
  left_join(country_list, join_by(location_name == english_formal)) |> 
  mutate(country = case_when(location_name == "Global" ~ "Global", 
                             .default = country)) |> 
  filter(!is.na(country))

## Maternal disorders - Level 4 causes ####
# **Query:**  
#   
#   **GBD estimate:** Cause of death or injury  
# **Measure:** Deaths; DALYs  
# **Metric:** Rate  
# **Cause:** Maternal disorders as well as all associated level 4 causes 
# **Location:** All countries and territories  
# **Age:** Age-standardized; 
# **Sex:** Female  
# **Year:** 2021 
# 
# This query generated a CSV file that I split by Deaths and DALYs and saved in RDS format in order to save diskspace.
# maternal_disorders <-
#   # List the filenames of each CSV file in the folder
#   list.files(path = here("data", "GBD", "maternal_disorders"),
#              pattern = "\\.csv$",
#              full.names = TRUE) |>
#   # Read them in using data.table::fread()
#   map_df(~data.table::fread(.)) |>
#   # Clean the names
#   janitor::clean_names() |>
#   # Format variables
#   mutate(
#     across(c(measure_name, location_name, sex_name,
#              cause_name, metric_name), as.factor),
#     date = ymd(paste0(year, "-01-01"))
#   ) |> 
#   mutate(age_name = fct_relevel(age_name, "Age-standardized")) |> 
#   filter(!age_name %in% c("65-69 years", "60-64 years")) |> 
#   droplevels()
# 
# # Split the file by measure
# maternal_disorders_DALY <- maternal_disorders |> filter(measure_name == "DALYs (Disability-Adjusted Life Years)")
# maternal_disorders_deaths <- maternal_disorders |> filter(measure_name == "Deaths")
# 
# # Save as RDS
# saveRDS(maternal_disorders_DALY, file = here("data", "GBD", "maternal_disorders", "maternal_disorders_DALY.rds"))
# saveRDS(maternal_disorders_deaths, file = here("data", "GBD", "maternal_disorders", "maternal_disorders_deaths.rds"))

maternal_disorders_DALY <- readRDS(here("data", "GBD", "maternal_disorders", "maternal_disorders_DALY.rds")) |> 
  left_join(country_list, join_by(location_name == english_formal)) |> 
  mutate(country = case_when(location_name == "Global" ~ "Global", 
                             .default = country)) |> 
  filter(!is.na(country))

maternal_disorders_deaths <- readRDS(here("data", "GBD", "maternal_disorders", "maternal_disorders_deaths.rds")) |> 
  left_join(country_list, join_by(location_name == english_formal)) |> 
  mutate(country = case_when(location_name == "Global" ~ "Global", 
                             .default = country)) |> 
  filter(!is.na(country))

# **Maternal haemorrhage** includes both postpartum haemorrhage (defined as blood loss ≥500 ml for vaginal delivery and ≥1000 ml for caesarean delivery) and antepartum haemorrhage (defined as vaginal bleeding from any cause at or beyond 20 weeks of gestation).  
# 
# **Maternal sepsis** is defined as a temperature <36°C or >38°C and clinical signs of shock (systolic blood pressure <90 mmHg and tachycardia >120 bpm). **Other maternal infections** are defined as any maternal infections excluding HIV, STI, or not related to pregnancy. 
# 
# **Maternal hypertensive disorders** include gestational hypertension (onset after 20 weeks gestation), pre-eclampsia, severe preeclampsia, and eclampsia, but exclude chronic hypertension (onset prior to pregnancy or prior to 20 weeks gestation) unless superimposed preeclampsia or eclampsia develop.
# 
# **Maternal obstructed labour and uterine rupture** aggregates obstructed labour (arrest in the first or second stage of active labour despite sufficient contractions), uterine rupture (non-surgical breakdown of uterine wall), and fistula (an abnormal opening between the vagina and the bladder or rectum following childbirth). 
# 
# **Abortion** is defined as elective or medically indicated termination of pregnancy at any gestational age. **Miscarriage** is defined as spontaneous loss of pregnancy before 24 weeks of gestation with complications requiring medical care.
# 
# **Ectopic pregnancy** is defined as pregnancy occurring outside of the uterus.
# 
# **Indirect maternal deaths** are due to existing diseases that are exacerbated by pregnancy. Examples include maternal infections and parasitic diseases complicating pregnancy, childbirth, and the puerperium, and diabetes in pregnancy, childbirth, and the puerperium. 
# 
# **Late maternal deaths** are deaths that occur six weeks to one year after the end of pregnancy, excluding incidental deaths.
# 
# **Maternal deaths aggravated by HIV/AIDS** are deaths occurring in HIV-positive women whose pregnancy has exacerbated their HIV/AIDS, leading to death.
# 
# **Other direct maternal disorders** encompasses a wide range of maternal disorders that do not map to other diseases in the GBD cause list, including other fatal or non-fatal complications occurring during pregnancy, childbirth, and the postpartum period. 
# 
# See also Cresswell et al. for more background on causes of maternal deaths:  
#   Cresswell, Jenny A, Monica Alexander, Michael Y C Chong, Heather M Link, Marija Pejchinovska, Ursula Gazeley, Sahar M A Ahmed, et al. “Global and Regional Causes of Maternal Deaths 2009–20: A WHO Systematic Analysis.” The Lancet Global Health 13, no. 4 (April 2025): e626–34. <https://doi.org/10.1016/S2214-109X(24)00560-6>.