


ireland_synth_table <- synth.tab(
  synth.res = ireland_synth,
  dataprep.res = ireland_prepped_data
)

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




# tab.pred table
ire_tbl <- as.data.frame(ireland_synth_table$tab.pred)
ire_tbl <- cbind(Variable = rownames(ire_tbl), ire_tbl)

flextable(ire_tbl) %>%
  set_header_labels(Variable = "")

