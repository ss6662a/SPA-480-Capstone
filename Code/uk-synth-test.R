
# failing to do...


selected_countries <- c("Ireland", "Sweden", "Norway", "United States", "Poland", 
                        "Italy", "New Zealand", "France", "Canada", "Brazil", "South Korea", "United Kingdom")

df_filtered <- full_data %>%
  filter(country %in% selected_countries & year <= 2012) %>%
  select(country, year, gdp, years_since_regime_change, ethnicity_index, p1_state_legitimacy)


df_avg <- df_filtered %>%
  group_by(country) %>%
  summarize(
    avg_gdp = mean(gdp, na.rm = TRUE),
    avg_regime_length = mean(years_since_regime_change, na.rm = TRUE),
    avg_ethnicity = mean(ethnicity_index, na.rm = TRUE),
    avg_legitimacy = mean(p1_state_legitimacy, na.rm = TRUE)
  )


# Extract UK values
uk_values <- df_avg %>% 
  filter(country == "United Kingdom") %>% 
  select(-country) %>% 
  as.numeric()

# Compute Euclidean distance for each country
df_avg <- df_avg %>%
  rowwise() %>%
  mutate(distance = sqrt(sum((c_across(avg_gdp:avg_legitimacy) - uk_values)^2, na.rm = TRUE))) %>%
  ungroup()

# Rank countries by similarity (lower distance = better match)
df_ranked <- df_avg %>%
  arrange(distance) %>%
  filter(country != "United Kingdom") %>%  # Remove UK from comparison
  drop_na()
  
print(df_ranked)


dataprep(foo = full_data_w_treat1,
         predictors = change_from_prev_year,
         dependent = fu)


full_data_wide <- full_data_no_na %>%
  pivot_wider(
    names_from = year, 
    values_from = c(p1_state_legitimacy, n_rai, gdp, years_since_regime_change),
    id_cols = country
    ) %>% 
  mutate(country_id = as.numeric(as.factor(country))) # makes a unit.variable

uk_prepped_data <- dataprep(
  foo = full_data_wide,
  predictors = c("homogeneity", "gdp", "regime_length"), 
  predictors.op = "mean",
  dependent = "p1_state_legitimacy", 
  unit.variable = "country_id", 
  time.variable = "year",
  treatment.identifier = "United Kingdom",
  controls.identifier = c("Canada", "Italy", "Sweden", "New Zealand", "Ireland", "Poland"),  # Choose control countries
  time.predictors.prior = c(2000:2011),  # Pre-treatment years
  time.optimize.ssr = c(2000:2011),  # Pre-treatment years used for optimization
  time.plot = c(2000:2020)  # Full period for visualization
)

panelview(p1_state_legitimacy ~ post_treat, 
          data = full_data_no_na, 
          index = c("country", "year"), 
          pre.post = TRUE)

