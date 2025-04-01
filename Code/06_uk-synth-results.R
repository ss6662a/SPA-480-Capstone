
uk_synth_table <- synth.tab(
  synth.res = uk_synth,
  dataprep.res = uk_prepped_data
)

uk_synth_path <- path.plot(
  synth.res = uk_synth,
  dataprep.res = uk_prepped_data
)

uk_synth_gap <- gaps.plot(
  synth.res = uk_synth,
  dataprep.res = uk_prepped_data
)

uk_synth_table
