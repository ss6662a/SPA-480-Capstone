

ireland_leg_plot <- ireland_treat %>% 
  filter(treated == 1) %>% 
  ggplot(aes(x = year, y = p1_state_legitimacy)) +
  geom_line(size = 1, color = "darkgreen") +
  geom_vline(
    xintercept = 2014,
    color = "blue",
    linetype = "dashed") +
  annotate("text",
    label = "Dramatic Change in RAI (Treatment)",
    x = 2016,
    y = 2,
    size = 3,
    color = "blue"
  ) +
  labs(
    title = "Ireland Legitimacy by Year",
    x = "Year",
    y = "Legitimacy",
    caption = "A lower score indicates a less fragile, more legitimate state"
  ) +
  theme(
    plot.caption = element_text(hjust = 0),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_continuous(breaks = seq(2006, 2018, by = 2))

ireland_leg_plot


ireland_rai_plot <- ireland_treat %>% 
  filter(treated == 1) %>% 
  ggplot(aes(x = year, y = n_rai)) +
  geom_line(size = 1, color = "darkgreen") +
  geom_vline(
    xintercept = 2014,
    color = "blue",
    linetype = "dashed") +
  annotate("text", 
           label = "Local Government Reform 
           Act went in to effect",
           x = 2015.5,
           y = 4,
           size = 3,
           color = "blue"
  ) +
  labs(
    title = "Ireland RAI by Year",
    x = "Year",
    y = "Regional Authority Index"
  ) +
  theme(
    plot.caption = element_text(hjust = 0),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_x_continuous(breaks = seq(2006, 2018, by = 2)) +
  scale_y_continuous(breaks = seq(min(ireland_treat$n_rai), 
                                  max(ireland_treat$n_rai), 
                                  by = 2))

ireland_rai_plot

ireland_rai_plot / ireland_leg_plot

