


ireland_treat <- full_data %>% 
  mutate(
    treated = if_else(country == "Ireland", 1, 0)
  ) %>% 
  mutate(country = as.character(country)) %>% 
  filter(year >= 2006 & year <= 2018) %>% 
  drop_na(!c(ethnicity_index, GTI, change_from_prev_year)) %>% 
  mutate(
    country_id =  as.numeric(as.factor(country)),
    .after = country
  )

colSums(is.na(ireland_treat))

# dropping countries that are missing GTI or Ethnicity index values:

ireland_treat %>% 
  group_by(country) %>% 
  filter(all(is.na(ethnicity_index)) | all(is.na(GTI))) %>%
  distinct(country, .keep_all = TRUE) %>% 
  select(country, country_id)

na_first_year <- c(31, 72) # Guyana and Trinidad and Tobago don't have 2006 data

na_countries = c(9, 27, 28, 31, 35, 43, 47, 56, 68, 75)

ireland_treat <- ireland_treat %>% 
  filter(!country_id %in% na_countries)

control_countries = setdiff(1:76, c(na_countries, na_first_year, 37)) #37 is ireland (treated unit)


ireland_prepped_data <- dataprep(
  foo = as.data.frame(ireland_treat),
  
  predictors = c("n_rai", "n_sharedrule", "n_selfrule"),
  
  predictors.op = "mean",
  
  special.predictors = list(
    list("gdp", 2006:2018, "mean"),
    list("years_since_regime_change", 2006:2018, "mean"),
    list("ethnicity_index", 2006:2013, "mean")#,
    #list("GTI", 2011:2018, "mean") # currently have zero values making the synth not run...need to figure out how to fix
  ),
  
  dependent = "p1_state_legitimacy",
  
  unit.variable = "country_id",
  unit.names.variable = "country",
  
  time.variable = "year",
  
  treatment.identifier = 37,
  
  controls.identifier = control_countries,
  
  time.predictors.prior = c(2006:2014),
  
  time.optimize.ssr = c(2006:2014),  # Pre-treatment years used for optimization
  time.plot = c(2006:2018)  # Full period for visualization
)

ireland_synth <- synth(ireland_prepped_data)


