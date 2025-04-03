

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


