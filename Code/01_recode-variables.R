

full_data <- reduce(
  list(FSI_full, 
       RAI, 
       regime_change, 
       ethnic_frac,
       GDP_per_cap),
  left_join,
  by = c("country", "year")
)

