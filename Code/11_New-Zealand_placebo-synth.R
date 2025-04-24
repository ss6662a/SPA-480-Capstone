

nz_treat <- full_data %>% 
  mutate(
    treated = if_else(country == "New Zealand", 1, 0)
  ) %>% 
  mutate(country = as.character(country)) %>% 
  filter(year >= 2006 & year <= 2018) %>% 
  drop_na(!c(ethnicity_index, GTI, change_from_prev_year)) %>% 
  mutate(
    country_id =  as.numeric(as.factor(country)),
    .after = country
  )

colSums(is.na(nz_treat))

# dropping countries that are missing GTI or Ethnicity index values:

nz_treat %>% 
  group_by(country) %>% 
  filter(all(is.na(ethnicity_index)) | all(is.na(GTI))) %>%
  distinct(country, .keep_all = TRUE) %>% 
  select(country, country_id)

na_first_year <- c(31, 72) # Guyana and Trinidad and Tobago don't have 2006 data

na_countries = c(9, 27, 28, 31, 35, 43, 47, 56, 68, 75)

nz_treat <- nz_treat %>% 
  filter(!country_id %in% na_countries)


control_countries = c("Finland",
                      "Slovenia", 
                      "Albania", 
                      "Chile",
                      "Singapore",
                      "Australia",
                      "Lithuania",
                      "Denmark")


nz_treat %>% 
  filter(country %in% control_countries) %>% 
  distinct(country, country_id)

control_country_ids <- c(1, 3, 14, 21, 26, 42, 64, 65)



nz_prepped_data <- dataprep(
  foo = as.data.frame(nz_treat),
  
  predictors = c("n_rai", "n_selfrule"),
  
  predictors.op = "mean",
  
  special.predictors = list(
    list("gdp", 2006:2018, "mean"),
    list("years_since_regime_change", 2006:2018, "mean"),
    list("ethnicity_index", 2006:2013, "mean"),
    list("GTI", 2011:2018, "mean") # currently have zero values making the synth not run...need to figure out how to fix
  ),
  
  dependent = "p1_state_legitimacy",
  
  unit.variable = "country_id",
  unit.names.variable = "country",
  
  time.variable = "year",
  
  treatment.identifier = 51,
  
  controls.identifier = control_country_ids,
  
  time.predictors.prior = c(2006:2014),
  
  time.optimize.ssr = c(2006:2014),  # Pre-treatment years used for optimization
  time.plot = c(2006:2018)  # Full period for visualization
)

nz_synth <- synth(nz_prepped_data)
nz_synth


nz_synth_table <- synth.tab(
  synth.res = nz_synth,
  dataprep.res = nz_prepped_data
)
nz_synth_table


nz_synth_path <- path.plot(
  synth.res = nz_synth,
  dataprep.res = nz_prepped_data,
  Ylab = c("Legitimacy"),
  Xlab = c("Year"),
  tr.intake = 2014,
  Main = "Synthetic Control - New Zealand"
)

# ggplot version with reversed y axis ----
actual <- nz_prepped_data$Y1plot
synthetic <- nz_prepped_data$Y0plot %*% nz_synth$solution.w
years <- nz_prepped_data$tag$time.plot

synth_df <- data.frame(
  year = years,
  actual = as.numeric(actual),
  synthetic = as.numeric(synthetic)
)

synth_long <- synth_df %>%
  pivot_longer(cols = c("actual", "synthetic"), names_to = "type", values_to = "legitimacy")


ggplot(synth_long, aes(x = year, y = legitimacy, linetype = type)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 2014, linetype = "dotted", size = 1) +
  scale_y_reverse() +
  labs(
    title = "Synthetic Control - New Zealand",
    y = "Legitimacy",
    x = "Year",
    caption = "Figure 5"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 1)
  )






nz_synth_gap <- gaps.plot(
  synth.res = nz_synth,
  dataprep.res = nz_prepped_data
)


nz_gaps <- nz_prepped_data$Y1plot - (nz_prepped_data$Y0plot %*% nz_synth$solution.w)
nz_gaps_pre <- nz_gaps[1:8, 1]
nz_gaps_post <- nz_gaps[9:13, 1]

nz_gaps_pre_avg <- mean(nz_gaps_pre) 
nz_gaps_post_avg <- mean(nz_gaps_post)

flextable(as.data.frame(cbind(
  Year = rownames(as.data.frame(nz_gaps_pre)), 
  nz_gaps_pre)))

flextable(as.data.frame(cbind(
  Year = rownames(as.data.frame(nz_gaps_post)), 
  nz_gaps_post)))


nz_gaps_post_avg - nz_gaps_pre_avg



# Differences of Pre and Post treatment values ----
ireland_gaps_pre_avg - nz_gaps_pre_avg


ireland_gaps_post_avg - nz_gaps_post_avg


