

full_data <- reduce(
  list(FSI_full, 
       RAI, 
       regime_change, 
       GDP_per_cap),
  left_join,
  by = c("country", "year")
)

