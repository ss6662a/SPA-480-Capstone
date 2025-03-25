

panel_view_data <- panelview(p1_state_legitimacy ~ post_treat, 
          data = treated_data_dec, 
          index = c("country", "year"), 
          pre.post = TRUE)


gsynth_counterfactual <- plot(gsynth_out_dec, type = 'counterfactual')

gsynth_raw <- plot(gsynth_out_dec, type = 'raw')
