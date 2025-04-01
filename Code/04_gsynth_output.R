
att_table <- data.frame(
  Period = as.numeric(names(gsynth_out_dec$att)),
  ATT = round(gsynth_out_dec$att, 4)
)

att_table <- flextable(att_table) %>%
  autofit()



# output <- stargazer(gsynth_out_dec, type = 'html')

