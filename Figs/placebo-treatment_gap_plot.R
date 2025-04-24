

gap_treated <- ireland_prepped_data$Y1plot - (ireland_prepped_data$Y0plot %*% ireland_synth$solution.w)
years <- 2006:2018

gap_df <- tibble(
  year = years,
  gap = as.numeric(gap_treated),
  unit = "Treated"
)


for (id in names(filtered_placebos)) {
  res <- filtered_placebos[[id]]
  gap_placebo <- res$dataprep$Y1plot - (res$dataprep$Y0plot %*% res$synth$solution.w)
  
  gap_df <- bind_rows(gap_df, tibble(
    year = years,
    gap = as.numeric(gap_placebo),
    unit = paste0("Placebo_", id)
  ))
}


ggplot(gap_df, aes(x = year, y = gap, group = unit)) +
  geom_line(data = subset(gap_df, grepl("Placebo", unit)),
            aes(color = "Placebos"), color = "gray", size = 0.5) +
  geom_line(data = subset(gap_df, unit == "Treated"),
            aes(color = "Treated"), color = "darkgreen", size = 1) +
  geom_vline(xintercept = 2014, linetype = "dashed") +
  labs(
    title = "Gap Between Treated/Placebo Units and Their Synthetic Models",
    y = "Gap (Actual - Synthetic Legitimacy)",
    x = "Year",
    caption = "Figure 4"
  ) +
  scale_y_reverse() +
  theme_minimal()




