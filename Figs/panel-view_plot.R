

panel_view_data <- panelview(p1_state_legitimacy ~ post_treat, 
          data = treated_data_dec, 
          index = c("country", "year"), 
          pre.post = TRUE,
          xlab = "Year",
          ylab = "Country",
          axis.lab.angle = 45,
          axis.lab.gap = c(0, 1.5),
          cex.axis.y = 5
          )
  
panel_view_data

# save(panel_view_data, file = here("Figs"))

