


ireland_synth_table <- synth.tab(
  synth.res = ireland_synth,
  dataprep.res = ireland_prepped_data
)
ireland_synth_table

ireland_synth_path <- path.plot(
  synth.res = ireland_synth,
  dataprep.res = ireland_prepped_data,
  Ylab = c("Legitimacy"),
  Xlab = c("Year"),
  tr.intake = 2014,
  Main = "Synthetic Control - Ireland"
)


ireland_synth_gap <- gaps.plot(
  synth.res = ireland_synth,
  dataprep.res = ireland_prepped_data
)




ire_tbl <- as.data.frame(ireland_synth_table$tab.pred)
ire_tbl <- cbind(Variable = rownames(ire_tbl), ire_tbl)

flextable(ire_tbl) %>%
  set_header_labels(Variable = "")



ire_tbl <- as.data.frame(ireland_synth_table$tab.w)

flextable(ire_tbl) %>%
  set_header_labels(Variable = "")






actual <- ireland_prepped_data$Y1plot
synthetic <- ireland_prepped_data$Y0plot %*% ireland_synth$solution.w
years <- ireland_prepped_data$tag$time.plot

synth_df <- data.frame(
  Year = years,
  Actual = as.numeric(actual),
  Synthetic = as.numeric(synthetic)
)

synth_long <- synth_df %>%
  pivot_longer(cols = c("actual", "synthetic"), names_to = "type", values_to = "legitimacy")


ggplot(synth_long, aes(x = Year, y = Legitimacy, linetype = Type)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 2014, linetype = "dotted", size = 1) +
  scale_y_reverse() + 
  labs(
    title = "Synthetic Control - Ireland",
    y = "Legitimacy",
    x = "Year",
    caption = "Figure 2"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 1)
  )



# Pre-T and Post-T differences ----

ireland_synth_gap <- gaps.plot(
  synth.res = ireland_synth,
  dataprep.res = ireland_prepped_data
)

ireland_gaps <- ireland_prepped_data$Y1plot - (ireland_prepped_data$Y0plot %*% ireland_synth$solution.w)
ireland_gaps_pre <- ireland_gaps[1:8, 1]
ireland_gaps_post <- ireland_gaps[9:13, 1]

ireland_gaps_pre_avg <- mean(ireland_gaps_pre)
ireland_gaps_post_avg <- mean(ireland_gaps_post)


# pre
flextable(as.data.frame(cbind(
  Year = rownames(as.data.frame(ireland_gaps_pre)), 
  ireland_gaps_pre)))

# post
flextable(as.data.frame(cbind(
  Year = rownames(as.data.frame(ireland_gaps_post)), 
  ireland_gaps_post)))

ireland_gaps_post_avg - ireland_gaps_pre_avg

