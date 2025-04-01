

full_data <- reduce(
  list(FSI_full, 
       RAI, 
       regime_change, 
       ethnic_frac,
       GDP_per_cap,
       GTI),
  left_join,
  by = c("country", "year")
) %>%
  mutate(
    country = as.factor(country),
    year = as.numeric(year),
    gdp = as.numeric(str_replace_all(gdp, ",", ""))
  ) %>%
  arrange(country, year) %>%
  distinct(country, year, .keep_all = TRUE) # gets rid of duplicates (there was at least one)

full_data_no_ethnicity_index <- full_data %>% 
  select(!"ethnicity_index") %>% 
  drop_na()

RAI_treat_level <- 2



treated_data_dec <- full_data_no_ethnicity_index %>% 
  mutate(treated = if_else(change_from_prev_year < (RAI_treat_level * -1), 1, 0))

treated_data_dec %>% filter(treated == 1) %>% distinct(country)

treated_years <- treated_data_dec %>%
  filter(treated == 1) %>% 
  group_by(country) %>% 
  summarize(first_treat_year = min(year))

treated_data_dec <- full_data_no_ethnicity_index %>%
  left_join(treated_years, by = "country") %>%
  mutate(post_treat = if_else(
    !is.na(first_treat_year) & year >= first_treat_year, 1, 0))

