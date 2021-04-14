library(ggplot2)
library(gganimate)
library(hrbrthemes)

main_power_second_panel1 <- electrical1 %>%
  filter(Key == "WL_371797")
main_power_second_panel2 <- electrical2 %>%
  filter(Key == "WL_371797")

main_power_second_panel1 = main_power_second_panel1[seq(1, nrow(main_power_second_panel1), 4), ]
main_power_second_panel2 = main_power_second_panel2[seq(1, nrow(main_power_second_panel2), 4), ]

main_power_second_panel1 <- main_power_second_panel1 %>% 
  separate(TS, c('date', 'time', "am_or_pm"), sep=" ")
main_power_second_panel2 <- main_power_second_panel2 %>% 
  separate(TS, c('date', 'time', "am_or_pm"), sep=" ")

quantile(main_power_second_panel1$Value, seq(0,1, by = 0.01)) #goes from 0 to 7,264
quantile(main_power_second_panel2$Value, seq(0,1, by = 0.01)) #goes from 0 to 3,916

main_power_second_stats1 <- main_power_second_panel1 %>%
  group_by(date) %>%
  summarize(count = n(),
            min_watts = min(Value),
            mean_watts = mean(Value),
            max_watts = max(Value))
main_power_second_stats2 <- main_power_second_panel2 %>%
  group_by(date) %>%
  summarize(count = n(),
            min_watts = min(Value),
            mean_watts = mean(Value),
            max_watts = max(Value))

main_power_second_stats <- rbind(main_power_second_stats1, main_power_second_stats2)

main_power_second_stats <- main_power_second_stats %>%
  inner_join(outdoor_temp_all)

cor(main_power_second_stats$mean_watts, main_power_second_stats$mean_temp) #-0.19

main_power_second_stats %>%
  ggplot( aes(x=mean_temp, y=mean_watts, color="blue")) +
  geom_line() +
  geom_point() +
  scale_color_viridis(discrete = TRUE) +
  labs(x = "Average Temperature",
       y = "Main Second Panel Average Watts",
       title = "Second Panel Watts and Temperature",
       subtitle = "Correlation coefficient of -0.71") +
  theme_minimal() +
  theme_tej() 

anim_save("main_power_second.gif")
