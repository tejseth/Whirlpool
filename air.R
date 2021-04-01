library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
library(hrbrthemes)
library(viridis)

theme_tej <- function() {
  theme(text = element_text(family='Tahoma', color="#232D4B"), # set font and color of all text
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))
}

air_quality <- read.csv("~/Downloads/whirlpool_air_quality.csv")
house_occupancy <- read.csv("~/Downloads/whirlpool_house_occupancy.csv")

air_quality <- air_quality %>%
  filter(!is.na(air_quality$TS))
air_quality <- air_quality %>%
  filter(TS != "")

air_quality <- air_quality %>% 
  separate(TS, c('date', 'time'), sep=" ")

air_quality$day_of_week <- weekdays(as.Date(air_quality$date))
colnames(air_quality)[which(names(air_quality) == "day_of_week")] <- "Day.of.Week"

air_stats <- air_quality %>%
  filter(!is.na(date)) %>%
  group_by(date) %>%
  summarize(min_value = min(Value),
           mean_value = mean(Value),
           max_value = max(Value))

air_house <- air_stats %>%
  left_join(house_occupancy, by = c("date" = "Date"))

#download the CSV to do some data cleaning
write.csv(air_house, 'air_house.csv')

air_house <- read.csv("~/Whirlpool/air_house.csv")

cor(air_house$mean_value, air_house$X..People.Recorded) #0.56!
cor(air_house$max_value, air_house$X..People.Recorded) #0.40

air_house %>%
  ggplot(aes(x = as.numeric(X..People.Recorded), y = mean_value, color = as.factor(X..People.Recorded))) +
  geom_jitter(aes(y = mean_value), 
              size = 3, width = 0.125, alpha=.5) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  scale_y_continuous(name = "Average Air Quality", 
                     breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(name = "House Occupancy", 
                     breaks = scales::pretty_breaks(n = 6)) +
  scale_color_brewer(palette="Set1") + # color points
  theme_minimal() +
  labs(title = "The Relationship Between Air Quality and House Occupancy",
       subtitle = "Correlation coefficient of 0.56") +
  theme_tej()
ggsave('air_house1.png', width = 16, height = 10, dpi = "retina")

air_house %>%
  ggplot(aes(x = as.numeric(X..People.Recorded), y = mean_value, fill = as.factor(X..People.Recorded))) +
  geom_boxplot() +
  scale_color_brewer(palette="Set1") + # color points
  scale_y_continuous(name = "Average Air Quality", 
                     breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(name = "House Occupancy", 
                     breaks = scales::pretty_breaks(n = 6)) +
  geom_jitter(color="black", size=0.3, alpha=0.2) +
  theme_minimal() +
  labs(title = "The Relationship Between Air Quality and House Occupancy",
       subtitle = "Correlation coefficient of 0.56") +
  theme_tej()
ggsave('air_house2.png', width = 16, height = 10, dpi = "retina")

air_house %>%
  ggplot(aes(x = Day.of.Week, y = mean_value, fill = as.factor(Day.of.Week))) +
  geom_boxplot() +
  scale_color_brewer(palette="Set1") + # color points
  scale_y_continuous(name = "Average Air Quality", 
                     breaks = scales::pretty_breaks(n = 10)) +
  geom_jitter(color="black", size=0.3, alpha=0.2) +
  theme_minimal() +
  labs(title = "The Relationship Between Air Quality and Day of the Week",
       x = "Day of the Week") +
  theme_tej()
ggsave('air_house3.png', width = 16, height = 10, dpi = "retina")

days_of_week <- air_house %>%
  group_by(Day.of.Week, X..People.Recorded) %>%
  summarize(count = n(),
            mean_air = mean(mean_value)) 

unique(air_quality$Device.Name)

