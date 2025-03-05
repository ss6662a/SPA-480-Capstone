library(tidyverse)
library(purrr)
library(here)
library(Synth)
library(gsynth)
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
  )


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
    country = as.factor(country),  # Convert to factor
    year = as.numeric(year)        # Convert to numeric
  ) %>%
  arrange(country, year) %>%  # Ensure data is sorted
  distinct(country, year, .keep_all = TRUE) %>%  # Remove duplicates
  drop_na()  # Remove rows with missing values

full_data2 %>%
  summarise(
    missing_values = sum(is.na(n_rai)),  # Count missing values
    non_numeric = sum(!is.numeric(n_rai))  # Check for non-numeric values
  )



gsynth_out <- gsynth(
  formula = p1_state_legitimacy ~ n_rai + gdp + ethnicity_index + years_since_regime_change,
  data = full_data2,
  index = c("country", "year"),
  force = "two-way",  # Includes country & time fixed effects
  CV = TRUE,          # Cross-validation for tuning
  r = 2        # Factor model (auto-selects)
  )
plot(gsynth_out)

full_data2 %>%
  select(country, year, n_rai) %>%
  filter(!is.finite(n_rai) | is.na(n_rai) | is.character(n_rai) | is.factor(n_rai)) %>%
  print(n = 20)  # Show up to 20 problem rows




