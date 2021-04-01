kitchen_lights <- kitchen_lights %>% 
  separate(TS, c('date', 'time', "am_or_pm"), sep=" ")

quantile(kitchen_lights$Value, seq(0,1, by = 0.01)) #anything above 26 is considered an outlier

kitchen_lights <- kitchen_lights %>%
  mutate(is_outlier = (ifelse(Value >= 26, TRUE, FALSE)))

kitchen_lights %>% filter(is_outlier == TRUE)

kitchen_lights_stats <- kitchen_lights %>%
  group_by(date) %>%
  summarize(count = n(), 
            min_watts = min(Value),
            mean_watts = mean(Value),
            max_watts = max(Value))

kitchen_occ <- kitchen_lights_stats %>%
  left_join(house_occupancy, by = c("date" = "Date"))

cor(kitchen_occ$mean_watts, kitchen_occ$X..People.Recorded) #0.49

kitchen_occ %>%
  ggplot(aes(x = as.numeric(X..People.Recorded), y = mean_watts, color = as.factor(X..People.Recorded))) +
  geom_jitter(aes(y = mean_watts), 
              size = 5, width = 0.075, alpha=.5) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  scale_y_continuous(name = "Average Watts Used in Kitchen", 
                     breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(name = "House Occupancy", 
                     breaks = scales::pretty_breaks(n = 6)) +
  scale_color_brewer(palette="Set1") + # color points
  theme_minimal() +
  labs(title = "The Relationship Between Watts in Kitchen and House Occupancy",
       subtitle = "Correlation coefficient of 0.49") +
  theme_tej() +
  annotate("rect", xmin = -0.15, xmax = 0.15, ymin = 3.3, ymax = 4.0, alpha = 0.25) +
  annotate("rect", xmin = 1.85, xmax = 2.15, ymin = 4.5, ymax = 6.1, alpha = 0.25) +
  annotate("rect", xmin = 2.85, xmax = 3.15, ymin = 5.5, ymax = 7.0, alpha = 0.25)
ggsave('kitchen_reach_1.png', width = 16, height = 10, dpi = "retina")