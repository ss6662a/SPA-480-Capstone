
full_data_no_na <- full_data %>% 
  select(!c("ethnicity_index", "GTI")) %>% #
  drop_na()

RAI_treat_level <- 2


treated_data_dec <- full_data_no_na %>% 
  mutate(treated = if_else(change_from_prev_year < (RAI_treat_level * -1), 1, 0))

treated_data_dec %>% filter(treated == 1) %>% distinct(country)

treated_years <- treated_data_dec %>%
  filter(treated == 1) %>% 
  group_by(country) %>% 
  summarize(first_treat_year = min(year))

treated_data_dec <- full_data_no_na %>%
  left_join(treated_years, by = "country") %>%
  mutate(post_treat = if_else(
    !is.na(first_treat_year) & year >= first_treat_year, 1, 0))




gsynth_out_dec <- gsynth(
  formula = p1_state_legitimacy ~ post_treat + gdp + years_since_regime_change,
  data = treated_data_dec,
  index = c("country", "year"),
  force = "two-way",  # Country & time fixed effects
  CV = TRUE,          # Cross-validation
  r = c(0, 3),
  inference = "parametric"
)

gsynth_out_dec
