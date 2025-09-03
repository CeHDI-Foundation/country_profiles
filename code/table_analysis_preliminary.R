mmr_model <-MMR |> 
  filter(TimeDimensionValue == "2019") |> 
  filter(SpatialDimType == "COUNTRY") |> 
  # filter(NumericValue>70) |> 
  rename(MMR = NumericValue) |> 
  select(MMR, country_name, SpatialDim)

abortion_law_model <- world_abortion_laws |> 
  # filter(category %in% c("IV. To Save the Mother's Life", "V. Prohibited Altogether")) |> 
  select(country, category)

abortion_rate_model <- abortion_rate |> 
  filter(SpatialDimType == "COUNTRY") |> 
  filter(Dim1 == "UNCERTAINTY_INTERVAL_UI95") |> 
  rename(abortion_rate = NumericValue) |> 
  select(country_name, abortion_rate)

unintended_pregnancy_model <- unintended_pregnancy |> 
  filter(SpatialDimType == "COUNTRY") |> 
  filter(Dim1 == "UNCERTAINTY_INTERVAL_UI95") |> 
  rename(unintended_pregnancy = NumericValue) |> 
  select(country_name, unintended_pregnancy)

family_planning_model <- family_planning |> 
  filter(SpatialDimType == "COUNTRY") |> 
  group_by(country_name) |> 
  slice_max(YEAR, n=1) |> 
  rename(family_planning = NumericValue) |> 
  select(family_planning, country_name)



a <- inner_join(mmr_high, abortion_restrictive, join_by(country_name == country)) |> 
  left_join(family_planning_model) |> 
  left_join(abortion_rate_model) |> 
  left_join(unintended_pregnancy_model) |> 
  left_join(state_geo |> select(country, income, subregion) |> st_drop_geometry(), 
            join_by(country_name == country)) |> 
  mutate(subregion = fct_relevel(subregion, "Western Europe"))

a |> group_by(category) |> summarise(mmr_mean = mean(MMR))
a |> count(category)

m1 <- glm(data = a, MMR ~ family_planning+income)
summary(m1)

m1 <- glm(data = a, MMR ~ family_planning+income)
summary(m1)
