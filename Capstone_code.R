library(tidyverse)
library(here)
here::here()


# Importing Data ----
RAI <- read_csv("SPA-480 Data/RAI_country-april-2021.csv")
regime_change <- read_csv("SPA-480 Data/Political-Regime-Data.csv")
GDP_per_cap <- read_csv("SPA-480 Data/GDP-per-capita_World-Bank.csv")

# Fragile States Index csv files ----
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
                      FSI_2023)

# Cleaning/Formatting Data----

# need to do:
# pivot_longer regime_change df
# figure wtf happened with the GDP df






# Joining Data-sets Together----



full_data <- left_join(FSI_full, RAI, by = c("Country", "Year")) %>%
  left_join(full_data) # need to finish


# Synth Stuff trying to get the treatment and control groups----





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



