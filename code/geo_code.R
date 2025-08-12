# Get member states geometries
state_geo <- necountries::ne_countries |> 
  # filter(type == "main"|country == "Alaska") #|>
  filter(status == "member"|status == "observer"|country == "Alaska"|country == "Greenland"|country=="Somaliland"|country == "Western Sahara") |> 
  select(iso2:sovereign, status, region:polygon)

# Combine US and Alaska
us_alaska <- state_geo |> 
  filter(sovereign == "United States of America") |> 
  st_cast("POLYGON") %>%
  mutate(area = st_area(.)) |> 
  group_by(country) |> 
  slice_max(order_by = area, n = 1) |> 
  ungroup() |> 
  st_union() |> st_sf() |> 
  rename("polygon" = 1) |> 
  mutate(country = "United States of America")

# Combine Somalia and Somaliland
somalia <- state_geo |> 
  filter(sovereign %in% c("Somalia", "Somaliland")) |> 
  st_union() |> st_sf() |> 
  rename("polygon" = 1) |> 
  mutate(country = "Somalia")

# Update geometry for US and Alaska
state_geo <- state_geo |> 
  mutate(polygon = case_when(iso3 == "USA" ~ us_alaska$polygon,
                             country == "Somalia" ~ somalia$polygon,
                             .default = polygon)) |> 
  filter(!country %in% c("Somaliland", "Alaska"))
# Get the centroid of each state and update dataset
point_centroid <- st_centroid(state_geo, of_largest_polygon = TRUE)
state_geo$point_centroid <- point_centroid$polygon
rm(us_alaska, somalia, point_centroid)

# Update state names for compatability with SDG dataset
state_geo <- state_geo |> 
  mutate(country = case_match(
    country,
    "Bolivia" ~ "Bolivia (Plurinational State of)",
    "Brunei" ~ "Brunei Darussalam",
    "D.R. Congo" ~ "Democratic Republic of the Congo",
    "East Timor" ~ "Timor-Leste",
    "Federated States of Micronesia" ~ "Micronesia (Federated States of)",
    "Iran" ~ "Iran (Islamic Republic of)" ,
    "Ivory Coast" ~ "C\u00f4te d'Ivoire",
    "Laos" ~ "Lao People's Democratic Republic",
    "Moldova" ~ "Republic of Moldova",
    "North Korea" ~ "Democratic People's Republic of Korea",
    "Russia" ~ "Russian Federation",
    "South Korea" ~ "Republic of Korea",
    "Syria" ~ "Syrian Arab Republic",
    "Tanzania" ~ "United Republic of Tanzania",
    "Turkey" ~ "T\u00fcrkiye",
    "United Kingdom" ~ "United Kingdom of Great Britain and Northern Ireland",
    "Venezuela" ~ "Venezuela (Bolivarian Republic of)",
    "Vietnam" ~ "Viet Nam",
    "eSwatini" ~ "Eswatini",
    .default = country
  )) |> 
  # st_transform(crs = 3857) |> 
  st_cast("MULTIPOLYGON")  # Recast geometry to all multipolygon rather than a mix, for downstream use with plotly

# UN states listing with offical names
# Downloaded from: https://unterm.un.org/unterm2/en/country
UN_official <- readxl::read_xlsx(here("data", "countries.xlsx")) |> 
  janitor::clean_names() |> 
  select(english_short, english_formal) |> 
  mutate(english_short = trimws(str_remove(english_short, "\\(the\\)|\\(The\\)"))) |> 
  mutate(english_short = case_when(
    english_short == "Netherlands (Kingdom of the)" ~ "Netherlands",
    english_short == "State of Palestine  *" ~ "Palestine",
    .default = english_short
  )) |> 
  mutate(english_formal = str_remove(english_formal, regex("^the\\b\\s+", ignore_case = TRUE)),
         english_formal = case_when(
           english_short == "Palestine" ~ "Palestine",
           english_short == "Guyana" ~ "Republic of Guyana",
           english_formal == "Republic of Türkiye" ~ "Republic of Turkey",
           english_short == "Bahamas" ~ "Commonwealth of the Bahamas",
           english_short == "North Macedonia" ~ "North Macedonia",
           english_short == "Sudan" ~ "Republic of Sudan",
           english_short == "Nepal" ~ "Federal Democratic Republic of Nepal",
           english_short == "Iceland" ~ "Republic of Iceland",
           .default = english_formal
         ))

state_geo <- left_join(state_geo, UN_official, join_by(country == english_short)) |> 
  mutate(english_formal = case_when(country == "Greenland" ~ "Greenland", .default = english_formal)) |> 
  rowid_to_column()

saveRDS(state_geo, here("output", "state_geo_enhanced.rds"))

state_geo_dist <- state_geo |> st_set_geometry("point_centroid") |> select(country) |> st_distance()
# give the rows & cols meaningful names
colnames(state_geo_dist) <- state_geo$country
rownames(state_geo_dist) <- state_geo$country
nearest_neighbors_list <- apply(state_geo_dist, 1, function(row_distances) {
  
  # 2. Sort the distances and get the original column indices
  sorted_indices <- order(row_distances)
  
  # 3. Get the names of the closest countries using the indices
  # We skip the first index [1] because it's the country itself (distance = 0).
  # We take the next 10 indices, from [2] to [11].
  closest_country_names <- colnames(state_geo_dist)[sorted_indices[2:30]]
  
  return(closest_country_names)
})
saveRDS(nearest_neighbors_list, here("output", "nearest_neighbors_list.rds"))

# country_row <-state_geo |> filter(country == "India") |> pull(rowid)
# nearest_neighbors_list[,country_row][1:11]

# List the ECSA-HC states
ecsa_states <- c("Kenya", "Lesotho", "Malawi", "Mauritius", "Eswatini", 
                 "United Republic of Tanzania", "Uganda", "Zambia", "Zimbabwe")


# # UN states listing with regional groupings
# # Downloaded from: https://unstats.un.org/unsd/methodology/m49/overview/
# 
# UNSD <- read_csv2(here("data", "UNSD_Methodology.csv")) |> 
#   janitor::clean_names() |> 
#   mutate(
#     intermediate_region_name = case_when(
#       is.na(intermediate_region_name) ~ sub_region_name, 
#             .default = intermediate_region_name),
#     least_developed_countries_ldc = case_when(
#       least_developed_countries_ldc == "x" ~ TRUE,
#       .default = FALSE)
#   ) |> 
#   select(intermediate_region_name, iso_alpha3_code, least_developed_countries_ldc)