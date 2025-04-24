
reg_data <- full_data %>% 
  group_by(country) %>% 
  mutate(
    year = fct(as.character(year))
  )

# Two-way Fixed Effects Regression ----
library(plm)

pdata <- pdata.frame(full_data, index = c("country", "year"))

model <- plm(p1_state_legitimacy ~ n_rai + gdp + years_since_regime_change + ethnicity_index, 
             data = pdata, 
             model = "within", 
             effect = "twoways")

summary(model)



lm_out <- lm(
    p1_state_legitimacy ~ n_rai + gdp + years_since_regime_change + ethnicity_index,
    data = reg_data
  )
summary(lm_out)

plot(lm_out)

# should I be using legitimacy or change in legitimacy?




 
 