

panel_view_data <- panelview(p1_state_legitimacy ~ post_treat, 
          data = treated_data_dec, 
          index = c("country", "year"), 
          pre.post = TRUE)


save(panel_view_data, file = here("Figs"))

