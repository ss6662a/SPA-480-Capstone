library(tidyverse)
library(purrr)
library(here)
library(Synth)
library(gsynth)
library(mice)
library(panelView)
here::here()


# RAI Data ----
RAI <- read_csv("SPA-480 Data/RAI_country-april-2021.csv") %>%
  janitor::clean_names() %>%
  rename(country = "country_name") %>%
  select(c("country",
           "year",
           "n_selfrule",
           "n_sharedrule",
           "n_rai")
  ) %>%
  group_by(country) %>%
  mutate(change_from_prev_year = n_rai - lag(n_rai))


# Fragile States Index csv files ----

FSI_folder <- "C:/Users/soren/OneDrive/Desktop/School/SPA-480 Capstone/SPA-480 Data/Fragile States Index"
# *** change to make it use the 'here' package later ***

FSI_full <- list.files(
  path = FSI_folder,
  pattern = "FSI-\\d{4}\\.csv$",
  full.names = TRUE
) %>%
  map_dfr(read_csv) %>%
  janitor::clean_names() %>%
  select(c("country", 
           "year", 
           "p1_state_legitimacy")
         )

# GDP Data ----
GDP_per_cap <- read_csv("SPA-480 Data/GDP-per-cap_WB.csv") %>%
  janitor::clean_names() %>%
  rename(country = "country_name") %>%
  select(!c("series_name",
            "series_code",
            "country_code")) %>% 
  pivot_longer(
    cols = !(country),
    names_to = "year",
    values_to = "gdp"
  ) %>%
  mutate(year = str_extract(year, "\\d{4}")) %>%
  mutate(year = as.numeric(year))


# Homogeneity Data ----
ethnic_frac <- read_csv("SPA-480 Data/Ethnic-Fractionalization_Data.csv") %>%
  janitor::clean_names() %>%
  rename(ethnicity_index = "e_findex") 


# Regime Change Data ----
regime_change <- read_csv("SPA-480 Data/Political-Regime_Data.csv") %>%
  filter(Indicator == "Polity database: Regime Durability Index") %>%
  pivot_longer(
    cols = !(`Economy ISO3`:Partner),
    names_to = "year",
    values_to = "years_since_regime_change"
  ) %>%
  rename(country = "Economy Name") %>%
  mutate(year = as.numeric(year)) %>% #Year is a character, this changes it so I can join the data together
  select(c("country",
           "year",
           "years_since_regime_change")
  ) %>%
  na.omit()


# Global Terrorism Data ----

# we'll see if they get back to me...




# Joining Data-sets Together----

full_data <- reduce(
  list(FSI_full, 
    RAI, 
    regime_change, 
    ethnic_frac,
    GDP_per_cap),
  left_join,
  by = c("country", "year")
  )


# Synth Stuff trying to get the treatment and control groups----

# need to use dataprep() to prep the data than can perform Synthetic control tests



# Generalized Synthetic Control (gsynth package)----

# Ensure correct data types
full_data2 <- full_data %>%
  mutate(
    country = as.factor(country),
    year = as.numeric(year),
    gdp = as.numeric(str_replace_all(gdp, ",", ""))
  ) %>%
  arrange(country, year) %>%
  distinct(country, year, .keep_all = TRUE)

colSums(is.na(full_data2))

full_data_no_na <- full_data2 %>%
  drop_na(c('change_from_prev_year', 'p1_state_legitimacy', 'ethnicity_index', 'years_since_regime_change')) %>% 
  filter(!(country %in% c("Guyana", "Trinidad and Tobago", "Serbia")))

colSums(is.na(full_data_no_na))


# ---- Viewing Dataset, to prepare for gsynth

full_data_no_na %>% 
  filter(change_from_prev_year != 0) %>% 
  summarize(mm = mean(change_from_prev_year))
# when there is a change, the average change is about 0.4 (so 3 can represent a significant change)

panelview(p1_state_legitimacy ~ change_from_prev_year + gdp + ethnicity_index + years_since_regime_change, 
          data = full_data2, 
          index = c("country", "year"), 
          pre.post =  TRUE)


panelview(p1_state_legitimacy ~ change_from_prev_year, 
          data = full_data2, 
          index = c("country", "year"), 
          type = "outcome")

RAI %>%
  filter(!change_from_prev_year < -3 | !change_from_prev_year > 3) %>%
  ggplot(aes(x = year, y = n_rai, color = country)) +
  geom_line() +
  theme(legend.position = "none") # graph

RAI %>%
  filter(change_from_prev_year < -3 | change_from_prev_year > 3) %>%
  ggplot(aes(x = year, y = n_rai, color = country)) +
  geom_line() +
  theme(legend.position = "none") # another graph



# ---- Treatment

full_data_w_treat <- full_data_no_na %>%
  mutate(treated = if_else(abs(change_from_prev_year) > 3, 1, 0))

treated_years <- full_data_w_treat %>%
  filter(treated == 1) %>% 
  group_by(country) %>% 
  summarize(first_treat_year = min(year))

full_data_no_na <- full_data_no_na %>%
  left_join(treated_years, by = "country") %>%
  mutate(post_treat = if_else(
      !is.na(first_treat_year) & year >= first_treat_year, 1, 0))




panelview(p1_state_legitimacy ~ post_treat, 
          data = full_data_no_na, 
          index = c("country", "year"), 
          pre.post = TRUE)

# ---- Gsynth

gsynth_out <- gsynth(
  formula = p1_state_legitimacy ~ post_treat + gdp + ethnicity_index + years_since_regime_change,
  data = full_data_no_na,
  index = c("country", "year"),
  force = "two-way",  # Includes country & time fixed effects
  CV = TRUE,          # Cross-validation
  r = c(0, 3)
  )
# works, but removes 4 of 6 countries due to lack of pre-treatment data, more countries or more years?


plot(gsynth_out, type = 'counterfactual')
plot(gsynth_out, type = 'gap')
plot(gsynth_out, type = 'raw')



