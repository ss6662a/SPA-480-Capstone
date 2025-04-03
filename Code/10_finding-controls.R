

ireland_sum <- ireland_treat %>% 
  filter(country == "Ireland") %>% 
  summarize(
    leg = mean(p1_state_legitimacy),
    rai = mean(n_rai[year < 2014]),
    gdp = mean(gdp),
    homogen = mean(ethnicity_index, na.rm = TRUE),
    gti = mean(GTI, na.rm = TRUE),
    regime = mean(years_since_regime_change)
  )


finding_controls <- ireland_treat %>%
  group_by(country) %>% 
  mutate(unfit = if_else(any(abs(change_from_prev_year) > 2), 1, 0))

finding_controls <- finding_controls %>% 
  filter(unfit == 0) %>% 
  group_by(country) %>% 
  summarize(
    leg = mean(p1_state_legitimacy),
    rai = mean(n_rai),
    change = mean(change_from_prev_year),
    gdp = mean(gdp),
    homogen = mean(ethnicity_index, na.rm = TRUE),
    gti = mean(GTI, na.rm = TRUE),
    regime = mean(years_since_regime_change)
  )

finding_controls %>% 
  filter(
    rai < 5 & rai > 1 
    )

# closest matches
c("Finland", "Sweden", "Slovenia", "Chile", "Portugal")
  
  
  
  
  

