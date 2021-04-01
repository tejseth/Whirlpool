temp_humidity_waterflow1 <- read_csv("~/Downloads/temp_humidity_waterflow1.csv")
temp_humidity_waterflow2 <- read_csv("~/Downloads/temp_humidity_waterflow2.csv")

outdoor_temp <- temp_humidity_waterflow1 %>%
  filter(Key == "WL_402")

outdoor_temp <- outdoor_temp %>%
  mutate(temp = Value*(9/5)+32)

outdoor_temp <- outdoor_temp %>% 
  separate(TS, c('date', 'time', "am_or_pm"), sep=" ")

outdoor_temp_avg <- outdoor_temp %>%
  group_by(date) %>%
  summarize(mean_temp = mean(temp, na.rm = T))

quantile(west_solar_1$Value, seq(0,1, by = 0.01)) #ranges from 0 to 4217

west_solar_1_small = west_solar_1[seq(1, nrow(west_solar_1), 3), ]

west_solar_1_small <- west_solar_1_small %>% 
  separate(TS, c('date', 'time', "am_or_pm"), sep=" ")

west_solar_1_stats <- west_solar_1_small %>%
  group_by(date) %>%
  summarize(count = n(),
            min_watts = min(Value),
            mean_watts = mean(Value),
            max_watts = max(Value))

west_solar_temp <- west_solar_1_stats %>%
  inner_join(outdoor_temp_avg)

cor(west_solar_temp$mean_watts, west_solar_temp$mean_temp) #0.60

west_solar_temp <- west_solar_temp %>% 
  separate(date, c('Month', 'day', "year"), sep= "/")

###
outdoor_temp2 <- temp_humidity_waterflow2 %>%
  filter(Key == "WL_402")

outdoor_temp2 <- outdoor_temp2 %>%
  mutate(temp = Value*(9/5)+32)

outdoor_temp2 <- outdoor_temp2 %>% 
  separate(TS, c('date', 'time', "am_or_pm"), sep=" ")

outdoor_temp_avg2 <- outdoor_temp2 %>%
  group_by(date) %>%
  summarize(mean_temp = mean(temp, na.rm = T))

west_solar_2 <- electrical2 %>%
  filter(Key == "WL_194886")

quantile(west_solar_2$Value, seq(0,1, by = 0.01)) #ranges from 0 to 3626

west_solar_2_small = west_solar_2[seq(1, nrow(west_solar_2), 3), ]

west_solar_2_small <- west_solar_2_small %>% 
  separate(TS, c('date', 'time', "am_or_pm"), sep=" ")

west_solar_2_stats <- west_solar_2_small %>%
  group_by(date) %>%
  summarize(count = n(),
            min_watts = min(Value),
            mean_watts = mean(Value),
            max_watts = max(Value))

###
west_solar_all_stats <- rbind(west_solar_1_stats, west_solar_2_stats)
outdoor_temp_all <- rbind(outdoor_temp_avg, outdoor_temp_avg2)

west_solar_all_temp <- west_solar_all_stats %>%
  inner_join(outdoor_temp_all)

cor(west_solar_all_temp$mean_watts, west_solar_all_temp$mean_temp) #0.63

west_solar_all_temp <- west_solar_all_temp %>% 
  separate(date, c('Month', 'day', "year"), sep= "/")

west_solar_all_temp$Month <- factor(west_solar_all_temp$Month, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
west_solar_all_temp %>%
  ggplot(aes(x = mean_temp, y = mean_watts, color = Month)) +
  geom_hline(yintercept = mean(west_solar_temp$mean_watts), color = "blue", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(west_solar_temp$mean_temp), color = "blue", linetype = "dashed", alpha=0.5) +
  geom_point(cex=4, alpha = .6) +
  annotate("segment", x = 5, xend = 85, y = -100, yend = 1290, colour = "black") +
  labs(x = "Average Temp. On Day",
       y = "Average West Solar Panel Watts On day)",
       title = "West Solar Panel Watts On Day Compared to Temperature",
       subtitle = "Correlation coefficient of 0.63") +
  theme_bw() + theme_tej() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(legend.position = "right")
ggsave('solar_panel_1.png', width = 16, height = 10, dpi = "retina")




