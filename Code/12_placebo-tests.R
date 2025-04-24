
placebo_ids <- c(3, 21, 26, 42, 51)
placebo_results <- list()

placebo_data <- full_data %>% 
  mutate(country = as.character(country)) %>% 
  filter(year >= 2006 & year <= 2018) %>% 
  drop_na(!c(ethnicity_index, GTI, change_from_prev_year)) %>% 
  mutate(
    country_id =  as.numeric(as.factor(country)),
    .after = country
  ) %>% 
  filter(
    country_id %in% placebo_ids
  )

for (i in placebo_ids) {
  dp <- dataprep(
    foo = as.data.frame(placebo_data),
    predictors = c("n_rai", "n_selfrule"),
    predictors.op = "mean",
    time.predictors.prior = c(2006:2014),
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
    treatment.identifier = i,
    controls.identifier = setdiff(unique(placebo_data$country_id), i),
    time.optimize.ssr = c(2006:2014),
    time.plot = c(2006:2018)
  )
  synth_result <- tryCatch({
    synth(dp)
  }, error = function(e) NULL)
  
  if (!is.null(synth_result)) {
    placebo_results[[as.character(i)]] <- list(
      synth = synth_result,
      dataprep = dp
    )
  }
}
