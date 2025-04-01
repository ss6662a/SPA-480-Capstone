
here()

# Regional Authority ----
RAI <- read_csv(here("Data", "RAI_country-april-2021.csv")) %>%
  janitor::clean_names() %>%
  rename(country = "country_name") %>%
  select(c("country",
           "year",
           "n_selfrule",
           "n_sharedrule",
           "n_rai")
  )

RAI <- RAI %>%
  group_by(country) %>%
  mutate(change_from_prev_year = n_rai - lag(n_rai))

# Legitimacy ----

FSI_folder <- here("Data", "Fragile States Index")
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


# GDP ----

GDP_per_cap <- read_csv(here("Data", "GDP-per-cap_WB.csv")) %>%
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

# Homogeneity ----

ethnic_frac <- read_csv(here("Data", "Ethnic-Fractionalization_Data.csv")) %>%
  janitor::clean_names() %>%
  rename(ethnicity_index = "e_findex") 


# Regime Length ----
regime_change <- read_csv(here("Data", "Political-Regime_Data.csv")) %>%
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

# Global Terrorism ----

GTI <- read_csv(here("Data", "Global-Terrorism-Data.csv")) %>% 
  select(c(1, seq(3, 30, by = 2))) %>% 
  drop_na() %>% 
  row_to_names(row_number = 1) %>% 
  pivot_longer(cols = !(country),
               names_to = "year",
               values_to = "GTI") %>% 
  mutate(year = str_replace(year, " score", "")) %>% 
  mutate(
    year = as.numeric(year),
    GTI = as.numeric(GTI)
  )





