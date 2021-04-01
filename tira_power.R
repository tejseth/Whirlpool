tira_main_power1 <- electrical1 %>%
  filter(Key == "WL_371817")
tira_main_power2 <- electrical2 %>%
  filter(Key == "WL_371817")

tira_main_power1 <- tira_main_power1 %>% 
  separate(TS, c('date', 'time', "am_or_pm"), sep=" ")
tira_main_power2 <- tira_main_power2 %>% 
  separate(TS, c('date', 'time', "am_or_pm"), sep=" ")

tira_main_power1 <- tira_main_power1 %>%
  filter(!is.na(Value))
tira_main_power2 <- tira_main_power2 %>%
  filter(!is.na(Value))

quantile(tira_main_power1$Value, seq(0,1, by = 0.01)) #goes from 0 to 7,858
quantile(tira_main_power2$Value, seq(0,1, by = 0.01)) #goes from 0 to 6252

tira_main_power_stats1 <- tira_main_power1 %>%
  group_by(date) %>%
  filter(!is.na(Value)) %>%
  summarize(count = n(),
            min_watts = min(Value),
            mean_watts = mean(Value),
            max_watts = max(Value))
tira_main_power_stats2 <- tira_main_power2 %>%
  group_by(date) %>%
  filter(!is.na(Value)) %>%
  summarize(count = n(),
            min_watts = min(Value),
            mean_watts = mean(Value),
            max_watts = max(Value))

tira_power_all_stats <- rbind(tira_main_power_stats1, tira_main_power_stats2)

tira_power_occ <- tira_power_all_stats %>%
  inner_join(house_occupancy, by = c("date" = "Date"))

cor(tira_power_occ$mean_watts, tira_power_occ$X..People.Recorded) #0.20

names(tira_power_occ)[names(tira_power_occ) == "X..People.Recorded"] <- "People"
tira_power_occ$People <- as.factor(tira_power_occ$People)

tira_power_occ %>%
  ggplot(aes(x=People, y=mean_watts, fill=People)) +
  geom_violin(width=1.4) +
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  labs(x = "Number of People in the House",
       y = "Tira Power | Average Watts",
       title = "Average Watts From Tira Power Strip With Number of People in the House",
       subtitle = "Correlation coefficient of 0.20") +
  theme_tej() +
  theme(legend.position="right")






