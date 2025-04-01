
# failing to do...


control_countries <- c("Ireland", "Sweden", "Norway", "Poland", 
                        "Italy", "New Zealand", "Canada", "Brazil")



uk_treat <- full_data %>% 
  mutate(
    treated = if_else(country == "United Kingdom", 1, 0)
    ) %>% 
  mutate(
   country_id =  as.numeric(as.factor(country)),
   .after = country
  ) %>%
  mutate(country = as.character(country)) %>% 
  filter(country %in% control_countries | country == "United Kingdom") %>% 
  filter(year >= 2006 & year <= 2018)

colSums(is.na(uk_treat))
  
  
control_country_ids <- uk_treat %>% 
  filter(country %in% control_countries) %>% 
  distinct(country_id) %>% 
  as.vector()


uk_prepped_data <- dataprep(
  foo = as.data.frame(uk_treat),
  
  predictors = c("n_rai", "n_sharedrule", "n_selfrule"),
  
  predictors.op = "mean",
  
  special.predictors = list(
    list("gdp", 2006:2018, "mean"),
    list("years_since_regime_change", 2006:2018, "mean"),
    list("ethnicity_index", 2006:2013, "mean"),
    list("GTI", 2011:2018, "mean")
  ),
  
  dependent = "p1_state_legitimacy",
  
  unit.variable = "country_id",
  unit.names.variable = "country",
  
  time.variable = "year",
  
  treatment.identifier = 179,
  
  controls.identifier = c(24, 32, 82, 85, 122, 128, 137, 164),
  
  time.predictors.prior = c(2006:2011),

  time.optimize.ssr = c(2006:2011),  # Pre-treatment years used for optimization
  time.plot = c(2006:2018)  # Full period for visualization
)

uk_synth <- synth(uk_prepped_data)

