main_power_main_panel1 <- electrical1 %>%
  filter(Key == "WL_194882")
main_power_main_panel2 <- electrical2 %>%
  filter(Key == "WL_194882")

main_power_main_panel1 = main_power_main_panel1[seq(1, nrow(main_power_main_panel1), 5), ]
main_power_main_panel2 = main_power_main_panel2[seq(1, nrow(main_power_main_panel2), 5), ]

main_power_main_panel1 <- main_power_main_panel1 %>% 
  separate(TS, c('date', 'time', "am_or_pm"), sep=" ")
main_power_main_panel2 <- main_power_main_panel2 %>% 
  separate(TS, c('date', 'time', "am_or_pm"), sep=" ")

quantile(main_power_main_panel1$Value, seq(0,1, by = 0.01)) #goes from 0 to 7,858
quantile(main_power_main_panel2$Value, seq(0,1, by = 0.01)) #goes from 0 to 6252

main_power_1_stats <- main_power_main_panel1 %>%
  group_by(date) %>%
  summarize(count = n(),
            min_watts = min(Value),
            mean_watts = mean(Value),
            max_watts = max(Value))
main_power_2_stats <- main_power_main_panel2 %>%
  group_by(date) %>%
  summarize(count = n(),
            min_watts = min(Value),
            mean_watts = mean(Value),
            max_watts = max(Value))

main_power_all_stats <- rbind(main_power_1_stats, main_power_2_stats)

main_power_occ <- main_power_all_stats %>%
  inner_join(house_occupancy, by = c("date" = "Date"))

cor(main_power_occ$mean_watts, main_power_occ$X..People.Recorded) #0.43

names(main_power_occ)[names(main_power_occ) == "X..People.Recorded"] <- "People"
main_power_occ$People <- as.factor(main_power_occ$People)

ggplot(main_power_occ, aes(x = mean_watts, y = People , fill = People)) +
  geom_density_ridges() +
  labs(x = "Main Power's Average Watts",
       y = "Number of People in House",
       title = "How Main Power's Watts Relates to Amount of People in the House",
       subtitle = "Correlation coefficient of 0.43") +
  theme_minimal() +
  theme_tej() +
  theme(legend.position = "right") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
ggsave('main_power_1.png', width = 16, height = 10, dpi = "retina")





