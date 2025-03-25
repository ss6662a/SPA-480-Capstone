

gsynth_out_dec <- gsynth(
  formula = p1_state_legitimacy ~ post_treat + gdp + years_since_regime_change,
  data = treated_data_dec,
  index = c("country", "year"),
  force = "two-way",  # Country & time fixed effects
  CV = TRUE,          # Cross-validation
  r = c(0, 3)
)

