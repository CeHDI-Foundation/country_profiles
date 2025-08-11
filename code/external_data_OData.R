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
  "ODataQuery", # More general API use of OData protocol
  "aphp/rgho" # Uses API to get data from Global Health Observatory
  # "PPgp/wpp2024" # United Nations World Population Prospects 2024
  # "m-muecke/isocountry" # Get ISO codes for countries
)

state_geo <- readRDS(here("output", "state_geo_enhanced.rds"))

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

# GHO ####
## API queries ####
gho_api <- ODataQuery::ODataQuery$new("https://ghoapi.azureedge.net/api")

## Metadata ####
# Indicators list
gho_indicators <- gho_api$path("Indicator")$retrieve()[[1]] |> select(-Language)

# Dimensions list
gho_dimensions <- gho_api$path("Dimension")$retrieve()[[1]]
# Countries list
country_codes <- gho_api$path("Dimension", "COUNTRY", "DimensionValues")$retrieve()[[1]] |> 
  select(Code, Title) |> 
  rename(COUNTRY = Code, country_name = Title)
# Region list
region_codes <- gho_api$path("Dimension", "REGION", "DimensionValues")$retrieve()[[1]] |> 
  select(Code, Title) |> 
  rename(REGION = Code, region_name = Title)
UN_region_codes <- gho_api$path("Dimension", "UNREGION", "DimensionValues")$retrieve()[[1]] |> 
  select(Code, Title) |> 
  rename(UNREGION = Code, UN_region_name = Title)
WB_income_codes <- gho_api$path("Dimension", "WORLDBANKINCOMEGROUP", "DimensionValues")$retrieve()[[1]] |> 
  select(Code, Title) |> 
  rename(WORLDBANKINCOMEGROUP = Code, WB_income_group = Title)

xxxcc <- gho_api$path("Dimension", "DHSMICSGEOREGION", "DimensionValues")$retrieve()[[1]]


## Search GHO codes ####
search_term <- "partum|natal"
gho_indicators |> filter(str_detect(IndicatorName, regex(search_term, ignore_case = TRUE))|
                      str_detect(IndicatorCode, regex(search_term, ignore_case = TRUE)))

## Indicators ####

### UHC ####
#### Get datasets ####
UHC_AVAILABILITY_SCORE <- gho_api$path("UHC_AVAILABILITY_SCORE")$retrieve()[[1]] |> tibble()
UHC_INDEX_REPORTED <- gho_api$path("UHC_INDEX_REPORTED")$retrieve()[[1]] |> tibble()
UHC_SCI_CAPACITY <- gho_api$path("UHC_SCI_CAPACITY")$retrieve()[[1]] |> tibble()
UHC_SCI_INFECT <- gho_api$path("UHC_SCI_INFECT")$retrieve()[[1]] |> tibble()
UHC_SCI_NCD <- gho_api$path("UHC_SCI_NCD")$retrieve()[[1]] |> tibble()
UHC_SCI_RMNCH <- gho_api$path("UHC_SCI_RMNCH")$retrieve()[[1]] |> tibble()
#### Combine into one ####
UHC_all <- bind_rows(UHC_INDEX_REPORTED, 
                     UHC_SCI_CAPACITY, UHC_SCI_INFECT,
                     UHC_SCI_NCD, UHC_SCI_RMNCH) |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim),
    UNREGION = case_when(SpatialDimType %in% c("UNREGION", "GLOBAL")~SpatialDim),
    WORLDBANKINCOMEGROUP = case_when(SpatialDimType %in% c("WORLDBANKINCOMEGROUP", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
left_join(country_codes) |> 
  left_join(region_codes) |> 
  left_join(UN_region_codes) |> 
  left_join(WB_income_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    NumericValue = as.numeric(NumericValue),
    year = ymd(paste0(YEAR, "-01-01"))
  )

## Maternal mortality ratio ####
MMR <- gho_api$path("MDG_0000000026")$retrieve()[[1]] |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
    left_join(country_codes) |> 
    left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
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

aanh <- gho_api$path("UNICEF_PNCMOTHER")$retrieve()[[1]]

## Health check after delivery ####
postnatal_care <- gho_api$path("UNICEF_PNCMOTHER")$retrieve()[[1]] |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  group_by(SpatialDim) |> 
  slice_max(order_by = as.numeric(YEAR), n=1) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  )

postnatal_care_5 <- gho_api$path("pncall5")$retrieve()[[1]] |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  ) |> 
  filter(Dim1 == "SEX_FMLE")

postnatal_care_3 <- gho_api$path("pncall3")$retrieve()[[1]] |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  ) 

# See: https://maternalhealthatlas.org/factsheets

## Births in health facility ####
institutional_birth <- gho_api$path("SRHINSTITUTIONALBIRTH")$retrieve()[[1]] |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  ) 

## HIV death ####
HIV_death <- gho_api$path("HIV_0000000006")$retrieve()[[1]] |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  ) 

## Family planning ####
### Need for family planning met ####
family_planning <- gho_api$path("SDGFPALL")$retrieve()[[1]] |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
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

### Modern contraceptive prevalence ####
contraceptive_prevalence <- gho_api$path("cpmowho")$retrieve()[[1]] |>
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  )

### Unintended pregnancy ####
unintended_pregnancy <- gho_api$path("SRH_PREGNANCY_UNINTENDED_RATE")$retrieve()[[1]] |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  )
### Abortion rate ####
abortion_rate <- gho_api$path("SRH_ABORTION_RATE")$retrieve()[[1]] |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  )

### Own informed decisions ####
informed_decisions <- gho_api$path("SG_DMK_SRCR_FN_ZS")$retrieve()[[1]] |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim),
    UNREGION = case_when(SpatialDimType %in% c("UNREGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  left_join(UN_region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  )

## Skilled birth ####
skilled_birth <- gho_api$path("MDG_0000000025")$retrieve()[[1]] |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
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

## HPV ####
### National program ####
HPV_national <- gho_api$path("NCD_CCS_hpv")$retrieve()[[1]] |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  ) 

### Coverage estimates ####
HPV_coverage <- gho_api$path("SDGHPVRECEIVED")$retrieve()[[1]] |> tibble() |> 
  mutate(
    COUNTRY = case_when(SpatialDimType == "COUNTRY" ~ SpatialDim),
    REGION = case_when(SpatialDimType %in% c("REGION", "GLOBAL")~SpatialDim),
    WORLDBANKINCOMEGROUP = case_when(SpatialDimType %in% c("WORLDBANKINCOMEGROUP", "GLOBAL")~SpatialDim)
  ) |> 
  left_join(gho_indicators) |> 
  left_join(country_codes) |> 
  left_join(region_codes) |> 
  left_join(WB_income_codes) |> 
  rename(YEAR = TimeDim) |> 
  mutate(
    # NumericValue = as.numeric(NumericValue),
    across(c(NumericValue:High), ~ as.numeric(.x)),
    year = ymd(paste0(YEAR, "-01-01"))
  )
