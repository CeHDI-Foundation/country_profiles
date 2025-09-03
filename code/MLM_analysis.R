# Prep data ------------------------------------------
start_year = 2005
dat_prep <- MMR |> 
  filter(!is.na(country_name)) |> 
  filter(country_name!="Cook Islands") |> 
  select(ParentLocation, COUNTRY, country_name, YEAR, NumericValue, mmr_cat) |> 
  # mutate(
  #   mmr_cat = factor(case_when(
  #     NumericValue < 100 ~ "<100",
  #     NumericValue < 300 ~ "<200",
  #     NumericValue >= 200 ~ "300+",
  #     .default = NA),
  #     levels = c("<100", "<200", "300+")
  #   )
  # ) |>
  mutate(NumericValue = round(NumericValue)) |> 
  # filter(YEAR %in% c(2005, 2010, 2015, 2018, 2023)) |> 
  filter(YEAR >= start_year) |>
  left_join(mmr_data_WHO, join_by(COUNTRY == country_iso_3_code, YEAR == year)) |> 
  rename(MMR = NumericValue) |> 
  left_join(institutional_birth |> 
              select(COUNTRY, YEAR, NumericValue) |> 
              rename(institutional_birth = NumericValue)) |> 
  left_join(skilled_birth |> 
              select(COUNTRY, YEAR, NumericValue) |> 
              rename(skilled_birth = NumericValue)) |> 
  left_join(family_planning |> 
              select(COUNTRY, YEAR, NumericValue) |> 
              rename(family_planning = NumericValue)) |> 
  left_join(NMIRF |> select(iso3, nmirf_classification), join_by(COUNTRY == iso3)) |> 
  mutate(nmirf_classification = fct_na_value_to_level(nmirf_classification, level = "Unknown"))

n_sup_mh <- sdg_data |> 
  # filter(state_under_review %in% c(mmr_most$country_name)) |> 
  mutate(state_under_review = factor(state_under_review)) |> 
  filter(response_upr %in% c("Supported", "Noted/Other")) |> 
  mutate(response_upr = fct_recode(response_upr, "Noted"="Noted/Other")) |> 
  group_by(state_under_review, response_upr) |> 
  summarise(nsup=sum(maternal_health != "Other")) |> 
  ungroup() |> 
  pivot_wider(names_from = response_upr,values_from = nsup) |> 
  mutate(
    support_ratio = case_when(Noted == 0 & Supported == 0 ~ NA,
                              Supported ==0 ~ 0,
                              Noted == 0 ~ Supported,
                              .default = Supported/Noted),
    perc_dec=case_when(Noted==0 & Supported==0 ~ NA,
                   .default= Supported/(Supported+Noted)),
    n_mh_recs = Noted+Supported,
    log_mh_recs = log(n_mh_recs+1),
    alt = support_ratio/perc_dec,
    perc=perc_dec*100,
    cat_mh_recs = factor(case_when(
      n_mh_recs < 5 ~ "<5",
      n_mh_recs < 10 ~ "5-9",
      n_mh_recs < 15 ~ "10-15",
      n_mh_recs >=15 ~ "15+"
    ))
  ) |> 
# group_by(state_under_review) |> 
#   mutate(perc = nsup/sum(nsup)*100,
#          nsup_tot = sum(nsup)) #|> 
# filter(response_upr == "Supported") |>
  left_join(state_geo, join_by(state_under_review == country)) |> 
  left_join(world_abortion_laws |> select(country, category), join_by(state_under_review==country)) |> 
  # select(iso3, nsup, perc, nsup_tot, pop, income, category, subregion, FCS_status) |> 
  ungroup() |> 
  mutate(
    perc_cat_50 = fct_relevel(case_when(
      perc >= 50 ~ "\u2265 50%",
      perc < 50 ~ "< 50%",
      .default = NA), "\u2265 50%"),
    
    perc_cat_60 = fct_relevel(case_when(
      perc >= 60 ~ "\u2265 60%",
      perc < 60 ~ "< 60%",
      .default = NA), "\u2265 60%"),
    
    perc_cat_65 = fct_relevel(case_when(
      perc >= 65 ~ "\u2265 65%",
      perc < 65 ~ "< 65%",
      .default = NA), "\u2265 65%"),
    
    perc_cat_70 = fct_relevel(case_when(
      perc >= 70 ~ "\u2265 70%",
      perc < 70 ~ "< 70%",
      .default = NA), "\u2265 70%"),
    
    perc_cat_80 = fct_relevel(case_when(
      perc >= 80 ~ "\u2265 80%",
      perc < 80 ~ "< 80%",
      .default = NA), "\u2265 80%"),
    
    perc_cat_90 = fct_relevel(case_when(
      perc >= 90 ~ "\u2265 90%",
      perc < 90 ~ "< 90%",
      .default = NA), "\u2265 90%"),
    
    perc_cat2 = factor(case_when(
      perc > 90 ~ "> 90%",
      perc >= 70 ~ "70% to 90%",
      perc < 70 ~ "< 70%",
      .default = NA
    ), levels = c("> 90%",
                  "70% to 90%",
                  "< 70%"))
  ) |>  select(iso3, Noted:cat_mh_recs, perc_cat_60:perc_cat2, income, category, region, subregion, wbregion, FCS_status)

dat_model <- left_join(dat_prep, n_sup_mh, join_by(COUNTRY == iso3)) |> 
  ungroup() |> 
  # filter(!is.na(perc_cat_60)) |>
  filter(country_name !="Georgia") |> 
  # filter(ParentLocation == "Africa") |>
  mutate(year = ymd(paste0(YEAR,"-01-01"))) |> 
  mutate(
    YEAR=YEAR-min(YEAR),
    KNOT = case_when(year<=ymd("2010-01-01") ~ 0, .default = 1)
    ) |> 
  arrange(country_name, year) |> 
  mutate(MMR=MMR, 
         livebirths_scaled = livebirths/mean(livebirths, na.rm=TRUE)
  )

high_mmr_states <- dat_model |> filter(YEAR==0) |> 
  select(country_name, MMR, mmr_cat) |> 
  rename(mmr_cat_baseline = mmr_cat, mmr_baseline = MMR)
dat_model <- dat_model |> left_join(high_mmr_states, join_by(country_name==country_name)) |> 
  mutate(
    mmr_cat_baseline = fct_rev(mmr_cat_baseline)
  ) 

## Update perc_cat variable ----
dat_model <- dat_model |> 
  # filter(!mmr_cat_baseline %in% c("<10", "10-19")) |> 
  mutate(perc_cat = perc_cat_60) |> 
  filter(!is.na(Noted)) |> 
  ungroup()

dat_model |> group_by(mmr_cat_baseline, perc_cat) |> 
  summarise(n=n_distinct(country_name)) |>
  pivot_wider(names_from = perc_cat, values_from = n)

legend_text <- str_wrap("Proportion of UPR recommendations related to maternal health that are supported by the State",width = 50)

# Initial explorations ---------------------------
## MMR -------
dat_model |> 
  mutate(YEAR=YEAR+start_year) |> 
  mutate(perc_cat = fct_rev(perc_cat)) |> 
  # filter()
  # group_by(YEAR) |> 
  # summarise(MMR = weighted.mean(MMR, w = livebirths, na.rm = TRUE)) |> 
  ggplot(aes(x = year, 
             # y=skilled_birth,
             y=MMR,
             # color = nmirf_classification,
             # fill = nmirf_classification
             , color = perc_cat,
             fill= perc_cat
  ))+
  labs(fill = NULL, color = NULL)+
  theme_bw()+
  labs(x = "Year", 
       y = "MMR estimate", 
       title = "Change in Maternal Mortality Ratio (MMR)\nFrom 2005 to 2023"
       ,fill = legend_text, color = legend_text
  )+
  # scale_color_manual(values = c("\u2265 60%" = "cyan3","< 60%" = "tomato3"))+
  # scale_fill_manual(values = c("\u2265 60%" = "cyan3","< 60%" = "tomato3"))+
  scale_y_continuous(limits=c(0, NA))+
  theme(
    legend.position = "inside",
    legend.position.inside = c(0,0),
    legend.justification = c(0,0),
    legend.background = element_rect(fill = "transparent"),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.title.y = element_text(size = 20),
    title = element_text(size = 15),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 20)
  )+
  facet_wrap(.~mmr_cat_baseline)+
  # facet_grid(rows = vars(nmirf_classification), cols = vars(mmr_cat_baseline))+
  # geom_path(aes(group=country_name), color = "grey")+
  # geom_line(aes(group = country_name), alpha = 0.2)+
  # geom_smooth(
  #   method = "lm", color = "red",
  # 
  #   aes(weight = livebirths_scaled)
  # )+
  geom_smooth(
    # method = "lm",
    
    aes(weight = livebirths_scaled)
  )#+facet_wrap(.~ParentLocation, scales="free")

m1 <- glm(
  data = dat_model, 
  MMR~YEAR*perc_cat*mmr_cat_baseline*KNOT
  , weights = livebirths_scaled
  )
summary(m1)
# Calculate the estimated marginal trends (slopes)

contrast(emtrends(m1, specs = ~ perc_cat
                  |mmr_cat_baseline
                  , var = "YEAR"), 
         method = "pairwise"
         , by = "mmr_cat_baseline"
)

## Skilled birth attendance ----
dat_model |> 
  mutate(YEAR=YEAR+2005) |> 
  mutate(perc_cat = fct_rev(perc_cat)) |> 
  # filter()
  # group_by(YEAR) |> 
  # summarise(MMR = weighted.mean(MMR, w = livebirths, na.rm = TRUE)) |> 
  ggplot(aes(x = year, 
             y=skilled_birth,
             # y=MMR
             , color = perc_cat,
             fill= perc_cat
  ))+
  labs(fill = NULL, color = NULL)+
  theme_bw()+
  labs(x = "Year", 
       y = "Skilled birth attendance (%)", 
       title = "Change in Skilled Birth Attendance\nFrom 2005 to 2023"
       ,fill = legend_text, color = legend_text
  )+
  # scale_color_manual(values = c("\u2265 60%" = "cyan3","< 60%" = "tomato3"))+
  # scale_fill_manual(values = c("\u2265 60%" = "cyan3","< 60%" = "tomato3"))+
  scale_y_continuous(limits=c(0, NA))+
  theme(
    legend.position = "inside",
    legend.position.inside = c(0,0),
    legend.justification = c(0,0),
    legend.background = element_rect(fill = "transparent"),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.title.y = element_text(size = 20),
    title = element_text(size = 15),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 20)
  )+
  facet_wrap(.~mmr_cat_baseline)+
  # geom_path(aes(group=country_name), color = "grey")+
  # geom_line(aes(group = country_name), alpha = 0.2)+
  geom_smooth(
    method = "lm",
    
    aes(weight = livebirths_scaled)
  )#+facet_wrap(.~ParentLocation, scales="free")

m_SB <- glm(
  data = dat_model, 
  skilled_birth~YEAR*perc_cat*mmr_cat_baseline, weights = livebirths_scaled)
summary(m_SB)
# Calculate the estimated marginal trends (slopes)

contrast(emtrends(m_SB, specs = ~ perc_cat
                  |mmr_cat_baseline
                  , var = "YEAR"), 
         method = "pairwise"
         , by = "mmr_cat_baseline"
)

## Institutional birth ----
dat_model |> 
  mutate(YEAR=YEAR+2005) |> 
  mutate(perc_cat = fct_rev(perc_cat)) |> 
  # filter()
  # group_by(YEAR) |> 
  # summarise(MMR = weighted.mean(MMR, w = livebirths, na.rm = TRUE)) |> 
  ggplot(aes(x = year, 
             y=institutional_birth,
             # y=MMR
             , color = perc_cat,
             fill= perc_cat
  ))+
  labs(fill = NULL, color = NULL)+
  theme_bw()+
  labs(x = "Year", 
       y = "Institutional birth (%)", 
       title = "Change in Institutional birth, from 2005 to 2023"
       ,fill = legend_text, color = legend_text
  )+
  # scale_color_manual(values = c("\u2265 60%" = "cyan3","< 60%" = "tomato3"))+
  # scale_fill_manual(values = c("\u2265 60%" = "cyan3","< 60%" = "tomato3"))+
  scale_y_continuous(limits=c(0, NA))+
  theme(
    legend.position = "inside",
    legend.position.inside = c(0,0),
    legend.justification = c(0,0),
    legend.background = element_rect(fill = "transparent"),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.title.y = element_text(size = 20),
    title = element_text(size = 15),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 20)
  )+
  facet_wrap(.~mmr_cat_baseline)+
  # geom_path(aes(group=country_name), color = "grey")+
  # geom_line(aes(group = country_name), alpha = 0.2)+
  geom_smooth(
    method = "lm",
    
    aes(weight = livebirths_scaled)
  )#+facet_wrap(.~ParentLocation, scales="free")

m_IB <- glm(
  data = dat_model, 
  institutional_birth~YEAR*perc_cat*mmr_cat_baseline, weights = livebirths_scaled)
summary(m_IB)
# plot(predictorEffects(m_IB))
# Calculate the estimated marginal trends (slopes)

contrast(emtrends(m_IB, specs = ~ perc_cat
                  |mmr_cat_baseline
                  , var = "YEAR"), 
         method = "pairwise"
         , by = "mmr_cat_baseline"
)

## Family planning ----
dat_model |> 
  mutate(YEAR=YEAR+2005) |> 
  mutate(perc_cat = fct_rev(perc_cat)) |> 
  # filter()
  # group_by(YEAR) |> 
  # summarise(MMR = weighted.mean(MMR, w = livebirths, na.rm = TRUE)) |> 
  ggplot(aes(x = year, 
             y=family_planning,
             # y=MMR
             , color = perc_cat,
             fill= perc_cat
  ))+
  labs(fill = NULL, color = NULL)+
  theme_bw()+
  labs(x = "Year", 
       y = "Need for family planning satisfied with modern methods (%)", 
       title = "Need for family planning satisfied with modern methods, from 2005 to 2023"
       ,fill = legend_text, color = legend_text
  )+
  # scale_color_manual(values = c("\u2265 60%" = "cyan3","< 60%" = "tomato3"))+
  # scale_fill_manual(values = c("\u2265 60%" = "cyan3","< 60%" = "tomato3"))+
  # scale_y_continuous(limits=c(0, NA))+
  theme(
    legend.position = "inside",
    legend.position.inside = c(0,0),
    legend.justification = c(0,0),
    legend.background = element_rect(fill = "transparent"),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.title.y = element_text(size = 20),
    title = element_text(size = 15),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 20)
  )+
  facet_wrap(.~mmr_cat_baseline)+
  # geom_path(aes(group=country_name), color = "grey")+
  # geom_line(aes(group = country_name), alpha = 0.2)+
  geom_smooth(
    method = "lm",
    
    aes(weight = livebirths_scaled)
  )#+facet_wrap(.~ParentLocation, scales="free")

m_FP <- glm(
  data = dat_model, 
  institutional_birth~YEAR*perc_cat*mmr_cat_baseline, weights = livebirths_scaled)
summary(m_FP)
# Calculate the estimated marginal trends (slopes)

contrast(emtrends(m_FP, specs = ~ perc_cat
                  |mmr_cat_baseline
                  , var = "YEAR"), 
         method = "pairwise"
         , by = "mmr_cat_baseline"
)
# Multilevel modelling ------------------------------------------
## MMR -----------------
m_MMR_0 <- lmer(MMR ~ 1+YEAR + (1+YEAR |country_name),
                data= dat_model #|> filter(mmr_cat_baseline %in% c("500+", "300-499", "100-299"))
                , weights = livebirths_scaled
                # , REML = FALSE
                # ,control = lmerControl(
                #   optimizer ='optimx', optCtrl=list(method='L-BFGS-B', maxit=2e6))
)

m_MMR_1 <- update(m_MMR_0, .~. + YEAR*perc_cat)
summary(m_MMR_1)
# Calculate the estimated marginal trends (slopes)
# This gives the slope of 'time' for each combination of policy and baseline_mmr
contrast(emtrends(m_MMR_1, specs = ~ perc_cat
                  # |mmr_cat_baseline
                  , var = "YEAR"),
         method = "pairwise"
         # , by = "mmr_cat_baseline"
         )

m_MMR_2 <- update(m_MMR_1, .~. + YEAR*perc_cat*mmr_cat_baseline)
summary(m_MMR_2)
# Calculate the estimated marginal trends (slopes)
# This gives the slope of 'time' for each combination of policy and baseline_mmr
contrast(emtrends(m_MMR_2, specs = ~ perc_cat
                  |mmr_cat_baseline
                  , var = "YEAR"),
         method = "pairwise"
         , by = "mmr_cat_baseline")

marginaleffects::plot_predictions(
  m_MMR_1, condition = c("YEAR", "perc_cat"),
  draw = FALSE, re.form = NA) |> mutate(YEAR = YEAR+2005) |>  
  mutate(perc_cat = fct_rev(perc_cat)) |> 
  ggplot(aes(x=YEAR, y = estimate, color = perc_cat, fill = perc_cat))+
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA)+
  theme_bw()+
  labs(x = "Year", y = "MMR estimate", 
       title = "Change in MMR over time, by supported % of\nrecommendations related to maternal health",
       fill = NULL, color = NULL)+
  # scale_color_manual(values = c("cyan3", "tomato3"))+
  # scale_fill_manual(values = c("cyan3", "tomato3"))+
  theme(
    legend.position = "inside",
    legend.position.inside = c(1,1),
    legend.justification = c(1,1),
    legend.background = element_rect(fill = "transparent"),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.title.y = element_text(size = 20),
    title = element_text(size = 15),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 20)
  )#+facet_wrap(.~mmr_cat_baseline)

ggsave(here("output", "MMR_mlm.svg"), width = 8, height=6)

marginaleffects::plot_predictions(
  m_MMR_2, condition = c("YEAR", "perc_cat", "mmr_cat_baseline"),
  draw = FALSE, re.form = NA) |> mutate(YEAR = YEAR+2005) |>  
  mutate(perc_cat = fct_rev(perc_cat)) |> 
  ggplot(aes(x=YEAR, y = estimate, color = perc_cat, fill = perc_cat))+
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA)+
  theme_bw()+
  labs(x = "Year", y = "MMR estimate", 
       title = "Change in MMR over time, by supported % of\nrecommendations related to maternal health",
       fill = NULL, color = NULL)+
  # scale_color_manual(values = c("cyan3", "tomato3"))+
  # scale_fill_manual(values = c("cyan3", "tomato3"))+
  theme(
    legend.position = "inside",
    legend.position.inside = c(1,1),
    legend.justification = c(1,1),
    legend.background = element_rect(fill = "transparent"),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.title.y = element_text(size = 20),
    title = element_text(size = 15),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 20)
  )+facet_wrap(.~mmr_cat_baseline)

ggsave(here("output", "MMR_mlm_baseline.svg"), width = 8, height=6)

## Skilled birth attendance ----
m_SB_0 <- lmer(skilled_birth ~ 1+YEAR + (1+YEAR |country_name),
                data= dat_model #|> filter(mmr_cat_baseline %in% c("500+", "300-499", "100-299"))
                , weights = livebirths_scaled
                # , REML = FALSE
                # ,control = lmerControl(
                #   optimizer ='optimx', optCtrl=list(method='L-BFGS-B', maxit=2e6))
)

m_SB_1 <- update(m_SB_0, .~. + YEAR*perc_cat)
summary(m_SB_1)
# Calculate the estimated marginal trends (slopes)
# This gives the slope of 'time' for each combination of policy and baseline_mmr
contrast(emtrends(m_SB_1, specs = ~ perc_cat
                  # |mmr_cat_baseline
                  , var = "YEAR"),
         method = "pairwise"
         # , by = "mmr_cat_baseline"
)

m_SB_2 <- update(m_SB_1, .~. + YEAR*perc_cat*mmr_cat_baseline)
summary(m_SB_2)
# Calculate the estimated marginal trends (slopes)
# This gives the slope of 'time' for each combination of policy and baseline_mmr
contrast(emtrends(m_SB_2, specs = ~ perc_cat
                  |mmr_cat_baseline
                  , var = "YEAR"),
         method = "pairwise"
         , by = "mmr_cat_baseline")

marginaleffects::plot_predictions(
  m_SB_1, condition = c("YEAR", "perc_cat"),
  draw = FALSE, re.form = NA) |> mutate(YEAR = YEAR+2005) |>  
  mutate(perc_cat = fct_rev(perc_cat)) |> 
  ggplot(aes(x=YEAR, y = estimate, color = perc_cat, fill = perc_cat))+
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA)+
  theme_bw()+
  labs(x = "Year", y = "Skilled birth attendance", 
       title = "Skilled birth attendance over time",
       fill = NULL, color = NULL)+
  # scale_color_manual(values = c("cyan3", "tomato3"))+
  # scale_fill_manual(values = c("cyan3", "tomato3"))+
  theme(
    legend.position = "inside",
    legend.position.inside = c(1,1),
    legend.justification = c(1,1),
    legend.background = element_rect(fill = "transparent"),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.title.y = element_text(size = 20),
    title = element_text(size = 15),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 20)
  )#+facet_wrap(.~mmr_cat_baseline)

ggsave(here("output", "SB_mlm.svg"), width = 8, height=6)

marginaleffects::plot_predictions(
  m_SB_2, condition = c("YEAR", "perc_cat", "mmr_cat_baseline"),
  draw = FALSE, re.form = NA) |> mutate(YEAR = YEAR+2005) |>  
  mutate(perc_cat = fct_rev(perc_cat)) |> 
  ggplot(aes(x=YEAR, y = estimate, color = perc_cat, fill = perc_cat))+
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA)+
  theme_bw()+
  labs(x = "Year", y = "Skilled birth attendance (%)", 
       title = "Skilled birth attendance over time",
       fill = NULL, color = NULL)+
  # scale_color_manual(values = c("cyan3", "tomato3"))+
  # scale_fill_manual(values = c("cyan3", "tomato3"))+
  theme(
    legend.position = "inside",
    legend.position.inside = c(1,1),
    legend.justification = c(1,1),
    legend.background = element_rect(fill = "transparent"),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.title.y = element_text(size = 20),
    title = element_text(size = 15),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 20)
  )+facet_wrap(.~mmr_cat_baseline)

ggsave(here("output", "SB_mlm_baseline.svg"), width = 8, height=6)


## Institutional birth ----
m_IB_0 <- lmer(institutional_birth ~ 1+YEAR + (1+YEAR |country_name),
               data= dat_model #|> filter(mmr_cat_baseline %in% c("500+", "300-499", "100-299"))
               , weights = livebirths_scaled
               # , REML = FALSE
               # ,control = lmerControl(
               #   optimizer ='optimx', optCtrl=list(method='L-BFGS-B', maxit=2e6))
)

m_IB_1 <- update(m_IB_0, .~. + YEAR*perc_cat)
summary(m_IB_1)
# Calculate the estimated marginal trends (slopes)
# This gives the slope of 'time' for each combination of policy and baseline_mmr
contrast(emtrends(m_IB_1, specs = ~ perc_cat
                  # |mmr_cat_baseline
                  , var = "YEAR"),
         method = "pairwise"
         # , by = "mmr_cat_baseline"
)

m_IB_2 <- update(m_IB_1, .~. + YEAR*perc_cat*mmr_cat_baseline)
summary(m_IB_2)
# Calculate the estimated marginal trends (slopes)
# This gives the slope of 'time' for each combination of policy and baseline_mmr
contrast(emtrends(m_IB_2, specs = ~ perc_cat
                  |mmr_cat_baseline
                  , var = "YEAR"),
         method = "pairwise"
         , by = "mmr_cat_baseline")

marginaleffects::plot_predictions(
  m_IB_1, condition = c("YEAR", "perc_cat"),
  draw = FALSE, re.form = NA) |> mutate(YEAR = YEAR+2005) |>  
  mutate(perc_cat = fct_rev(perc_cat)) |> 
  ggplot(aes(x=YEAR, y = estimate, color = perc_cat, fill = perc_cat))+
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA)+
  theme_bw()+
  labs(x = "Year", y = "Institutional births (%)", 
       title = "Institutional births over time",
       fill = NULL, color = NULL)+
  # scale_color_manual(values = c("cyan3", "tomato3"))+
  # scale_fill_manual(values = c("cyan3", "tomato3"))+
  theme(
    legend.position = "inside",
    legend.position.inside = c(1,1),
    legend.justification = c(1,1),
    legend.background = element_rect(fill = "transparent"),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.title.y = element_text(size = 20),
    title = element_text(size = 15),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 20)
  )#+facet_wrap(.~mmr_cat_baseline)

ggsave(here("output", "IB_mlm.svg"), width = 8, height=6)

marginaleffects::plot_predictions(
  m_IB_2, condition = c("YEAR", "perc_cat", "mmr_cat_baseline"),
  draw = FALSE, re.form = NA) |> mutate(YEAR = YEAR+2005) |>  
  mutate(perc_cat = fct_rev(perc_cat)) |> 
  ggplot(aes(x=YEAR, y = estimate, color = perc_cat, fill = perc_cat))+
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA)+
  theme_bw()+
  labs(x = "Year", y = "Institutional births (%)", 
       title = "Institutional births over time",
       fill = NULL, color = NULL)+
  # scale_color_manual(values = c("cyan3", "tomato3"))+
  # scale_fill_manual(values = c("cyan3", "tomato3"))+
  theme(
    legend.position = "inside",
    legend.position.inside = c(1,1),
    legend.justification = c(1,1),
    legend.background = element_rect(fill = "transparent"),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.title.y = element_text(size = 20),
    title = element_text(size = 15),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 20)
  )+facet_wrap(.~mmr_cat_baseline)

ggsave(here("output", "IB_mlm_baseline.svg"), width = 8, height=6)


## Family planning ----
m_FP_0 <- lmer(family_planning ~ 1+YEAR + (1+YEAR |country_name),
               data= dat_model #|> filter(mmr_cat_baseline %in% c("500+", "300-499", "100-299"))
               , weights = livebirths_scaled
               # , REML = FALSE
               # ,control = lmerControl(
               #   optimizer ='optimx', optCtrl=list(method='L-BFGS-B', maxit=2e6))
)

m_FP_1 <- update(m_FP_0, .~. + YEAR*perc_cat)
summary(m_FP_1)
# Calculate the estimated marginal trends (slopes)
# This gives the slope of 'time' for each combination of policy and baseline_mmr
contrast(emtrends(m_FP_1, specs = ~ perc_cat
                  # |mmr_cat_baseline
                  , var = "YEAR"),
         method = "pairwise"
         # , by = "mmr_cat_baseline"
)

m_FP_2 <- update(m_FP_1, .~. + YEAR*perc_cat*mmr_cat_baseline)
summary(m_FP_2)
# Calculate the estimated marginal trends (slopes)
# This gives the slope of 'time' for each combination of policy and baseline_mmr
contrast(emtrends(m_FP_2, specs = ~ perc_cat
                  |mmr_cat_baseline
                  , var = "YEAR"),
         method = "pairwise"
         , by = "mmr_cat_baseline")

marginaleffects::plot_predictions(
  m_FP_1, condition = c("YEAR", "perc_cat"),
  draw = FALSE, re.form = NA) |> mutate(YEAR = YEAR+2005) |>  
  mutate(perc_cat = fct_rev(perc_cat)) |> 
  ggplot(aes(x=YEAR, y = estimate, color = perc_cat, fill = perc_cat))+
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA)+
  theme_bw()+
  labs(x = "Year", y = "Need for family planning satisfied with modern methods (%)", 
       title = "Satisfied need for family planning over time",
       fill = NULL, color = NULL)+
  # scale_color_manual(values = c("cyan3", "tomato3"))+
  # scale_fill_manual(values = c("cyan3", "tomato3"))+
  theme(
    legend.position = "inside",
    legend.position.inside = c(1,1),
    legend.justification = c(1,1),
    legend.background = element_rect(fill = "transparent"),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.title.y = element_text(size = 20),
    title = element_text(size = 15),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 20)
  )#+facet_wrap(.~mmr_cat_baseline)

ggsave(here("output", "FP_mlm.svg"), width = 8, height=6)

marginaleffects::plot_predictions(
  m_FP_2, condition = c("YEAR", "perc_cat", "mmr_cat_baseline"),
  draw = FALSE, re.form = NA) |> mutate(YEAR = YEAR+2005) |>  
  mutate(perc_cat = fct_rev(perc_cat)) |> 
  ggplot(aes(x=YEAR, y = estimate, color = perc_cat, fill = perc_cat))+
  geom_line()+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA)+
  theme_bw()+
  labs(x = "Year", y = "Need for family planning satisfied with modern methods (%)", 
       title = "Satisfied need for family planning over time",
       fill = NULL, color = NULL)+
  # scale_color_manual(values = c("cyan3", "tomato3"))+
  # scale_fill_manual(values = c("cyan3", "tomato3"))+
  theme(
    legend.position = "inside",
    legend.position.inside = c(1,1),
    legend.justification = c(1,1),
    legend.background = element_rect(fill = "transparent"),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.title.y = element_text(size = 20),
    title = element_text(size = 15),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 20)
  )+facet_wrap(.~mmr_cat_baseline)

ggsave(here("output", "FP_mlm_baseline.svg"), width = 8, height=6)