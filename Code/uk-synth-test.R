
# failing to do...


control_countries <- c("Ireland", "Sweden", "Norway", "United States", "Poland", 
                        "Italy", "New Zealand", "France", "Canada", "Brazil", "South Korea")



uk_treat <- full_data %>% 
  mutate(
    treated = if_else(country == "United Kingdom", 1, 0)
    ) %>% 
  mutate(
   country_id =  as.numeric(as.factor(country)),
   .after = country
  ) %>% 
  filter(country %in% control_countries | country == "United Kingdom") %>% 
  
  
  
control_country_ids <- uk_treat %>% 
  filter(country %in% control_countries) %>% 
  distinct(country_id) %>% 
  as.vector()

# filter out NA values

uk_prepped_data <- dataprep(
  foo = uk_treat,
  
  predictors = c(
    "homogeneity", 
    "gdp", 
    "regime_length", 
    "ethnicity_index"),
  
  predictors.op = "mean",
  
  special.predictors = list(
    list("ethnicity_index", 2006:2013, "mean")
  ),
  
  dependent = "p1_state_legitimacy",
  
  unit.variable = "country_id",
  unit.names.variable = "country",
  
  time.variable = "year",
  
  treatment.identifier = 179,
  
  controls.identifier = c(24, 32, 62, 82, 85, 122, 128, 137, 157, 164, 180),
  
  time.predictors.prior = c(2006:2011),

  time.optimize.ssr = c(2006:2011),  # Pre-treatment years used for optimization
  time.plot = c(2006:2018)  # Full period for visualization
)
colSums(is.na(uk_treat))
