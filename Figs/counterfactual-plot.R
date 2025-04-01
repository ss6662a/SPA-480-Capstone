
gsynth_counterfactual <- plot(gsynth_out_dec, type = 'counterfactual') +
  labs(
    title = "Gsynth Counterfactuals",
    subtitle = "For countries that had a significant frop in RAI",
    x = "Year",
    y = "Legitimacy",
    caption = "Lower legitimacy score indicates less fragile and more legitimate"
  ) +
  theme(
    plot.caption = element_text(hjust = 0),
    plot.title = element_text(hjust = 0)
  )

gsynth_counterfactual


#save(gsynth_counterfactual, file = here("Figs"))
