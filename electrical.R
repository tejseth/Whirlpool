library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
library(hrbrthemes)
library(viridis)
library(gt)
library(dplyr)
library(ggridges)
library(ggplot2)

theme_tej <- function() {
  theme(text = element_text(family='Tahoma', color="#232D4B"), # set font and color of all text
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))
}

electrical1 <- read_csv("~/Downloads/electrical1.csv")
electrical2 <- read_csv("~/Downloads/electrical2.csv")
electrical3 <- read_csv("~/Downloads/electrical3.csv")

electrical1 <- electrical1 %>%
  filter(!is.na(electrical1$TS))
electrical1 <- electrical1 %>%
  filter(TS != "")

unique(electrical1$Key)

main_power_main_panel <- electrical1 %>%
  filter(Key == "WL_194882")
west_solar_1 <- electrical1 %>%
  filter(Key == "WL_194886")
kitchen_lights <- electrical1 %>%
  filter(Key == "WL_194890")
water_heater <- electrical1 %>%
  filter(Key == "WL_194895")
fridge <- electrical1 %>%
  filter(Key == "WL_194903")
main_power_second_panel <- electrical1 %>%
  filter(Key == "WL_371797")
rain_water_sump_pump <- electrical1 %>%
  filter(Key == "WL_371801")
tira_main_power <- electrical1 %>%
  filter(Key == "WL_371817")



