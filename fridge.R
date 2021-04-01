quantile(fridge$Value, seq(0,1, by = 0.001)) #ranges from 0 to 1069

fridge <- fridge %>% 
  separate(TS, c('date', 'time', "am_or_pm"), sep=" ")

fridge_stats <- fridge %>%
  filter(Value < 500) %>%
  group_by(date) %>%
  summarize(count = n(),
            min_watts = min(Value),
            mean_watts = mean(Value),
            max_watts = max(Value)) %>%
  filter(mean_watts > 30)

fridge_occ <- fridge_stats %>%
  left_join(house_occupancy, by = c("date" = "Date"))

cor(fridge_occ$mean_watts, fridge_occ$X..People.Recorded) #-0.14

fridge_occ %>%
  ggplot(aes(x = as.numeric(X..People.Recorded), y = mean_watts, fill = as.factor(X..People.Recorded))) +
  geom_boxplot() +
  scale_color_brewer(palette="Set1") + # color points
  scale_y_continuous(name = "Average Watts From Fridge", 
                     breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(name = "House Occupancy", 
                     breaks = scales::pretty_breaks(n = 6)) +
  geom_jitter(color="black", size=0.75, alpha=0.4) +
  theme_minimal() +
  labs(title = "The Relationship Between Fridge Watt Usage and House Occupancy",
       subtitle = "Correlation coefficient of -0.14") +
  theme_tej()
ggsave('fridge1.png', width = 16, height = 10, dpi = "retina")


