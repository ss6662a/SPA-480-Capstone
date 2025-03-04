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


# Random Stuff...idk----
unique_country = c()
score_dif = c()

for(i in data$country_name) {
  
  if (i %in% unique_country) {
    print("no")
  }
  
  else{
    
    unique_country <- append(unique_country, i, after = length(unique_country))
    print(i)
    
  }
}

# unfinished, trying to get the difference of the max and min RAI scores ----
test = c()
for (i in RAI$country_name){
  
  if(i %in% test) {
    print("no")
  }
  else {
    
    max_score = max(RAI$n_RAI[data$country_name == i])
    min_score = min(data$n_RAI[data$country_name == i])
    dif = abs(max_score - min_score)
    
    score_dif <- append(score_dif, dif)
    test <- append(test, i)
  }
}


data2 <- data %>%
  select(country_name, year, n_RAI) %>%
  group_by(country_name) %>%
  mutate(RAI_change = n_RAI - lag(n_RAI)) %>%
  ungroup()

data2 %>%
  arrange(desc(RAI_change)) %>%
  slice_head(n = 20)




# random snipets, delete later

for (file in FSI_files) {
  janitor::clean_names() %>%
    select(c(
      "country",
      "year",
      "p1_state_legitimacy")
    )
}


FSI_2006 <- read_csv("SPA-480 Data/Fragile States Index/FSI-2006.csv")
FSI_2007 <- read_csv("SPA-480 Data/Fragile States Index/FSI-2007.csv")
FSI_2008 <- read_csv("SPA-480 Data/Fragile States Index/FSI-2008.csv")
FSI_2009 <- read_csv("SPA-480 Data/Fragile States Index/FSI-2009.csv")
FSI_2010 <- read_csv("SPA-480 Data/Fragile States Index/FSI-2010.csv")
FSI_2011 <- read_csv("SPA-480 Data/Fragile States Index/FSI-2011.csv")
FSI_2012 <- read_csv("SPA-480 Data/Fragile States Index/FSI-2012.csv")
FSI_2013 <- read_csv("SPA-480 Data/Fragile States Index/FSI-2013.csv")
FSI_2014 <- read_csv("SPA-480 Data/Fragile States Index/FSI-2014.csv")
FSI_2015 <- read_csv("SPA-480 Data/Fragile States Index/FSI-2015.csv")
FSI_2016 <- read_csv("SPA-480 Data/Fragile States Index/FSI-2016.csv")
FSI_2017 <- read_csv("SPA-480 Data/Fragile States Index/FSI-2017.csv")
FSI_2018 <- read_csv("SPA-480 Data/Fragile States Index/FSI-2018.csv")
FSI_2019 <- read_csv("SPA-480 Data/Fragile States Index/FSI-2019.csv")
FSI_2020 <- read_csv("SPA-480 Data/Fragile States Index/FSI-2020.csv")
FSI_2021 <- read_csv("SPA-480 Data/Fragile States Index/FSI-2021.csv")
FSI_2022 <- read_csv("SPA-480 Data/Fragile States Index/FSI-2022.csv")
FSI_2023 <- read_csv("SPA-480 Data/Fragile States Index/FSI-2023.csv")


FSI_full <- bind_rows(FSI_2006,
                      FSI_2007,
                      FSI_2008,
                      FSI_2009,
                      FSI_2010,
                      FSI_2011,
                      FSI_2012,
                      FSI_2013,
                      FSI_2014,
                      FSI_2015,
                      FSI_2016,
                      FSI_2017,
                      FSI_2018,
                      FSI_2019,
                      FSI_2020,
                      FSI_2021,
                      FSI_2022,
                      FSI_2023) %>%
  janitor::clean_names() %>%
  select(c("country", 
           "year", 
           "p1_state_legitimacy"))

