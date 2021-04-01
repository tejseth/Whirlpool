rain_water_sump_pump1 <- electrical1 %>%
  filter(Key == "WL_371801")
rain_water_sump_pump2 <- electrical2 %>%
  filter(Key == "WL_371801")

rain_water1 <- temp_humidity_waterflow1 %>%
  filter(Key == "WL_909")
rain_water2 <- temp_humidity_waterflow2 %>%
  filter(Key == "WL_909")

rain_water_sump_pump1 = rain_water_sump_pump1[seq(1, nrow(rain_water_sump_pump1), 3), ]
rain_water_sump_pump2 = rain_water_sump_pump2[seq(1, nrow(rain_water_sump_pump2), 3), ]

rain_water_sump_pump1 <- rain_water_sump_pump1 %>% 
  separate(TS, c('date', 'time', "am_or_pm"), sep=" ")
rain_water_sump_pump2 <- rain_water_sump_pump2 %>% 
  separate(TS, c('date', 'time', "am_or_pm"), sep=" ")
rain_water1 <- rain_water1 %>% 
  separate(TS, c('date', 'time', "am_or_pm"), sep=" ")
rain_water2 <- rain_water2 %>% 
  separate(TS, c('date', 'time', "am_or_pm"), sep=" ")

quantile(rain_water_sump_pump1$Value, seq(0,1, by = 0.001)) #ranges from 0 to 22 with outliers going from 826 to 1152
quantile(rain_water_sump_pump2$Value, seq(0,1, by = 0.001)) #ranges from 0 to 22 with outliers going from 555 to 1129
quantile(rain_water1$Value, seq(0,1, by = 0.01)) #ranges from 0 to 45
quantile(rain_water2$Value, seq(0,1, by = 0.01)) #totally messed up...

rain_water_sump_pump1 <- rain_water_sump_pump1 %>%
  mutate(Value = ifelse(Value > 200, 200, Value))
rain_water_sump_pump2 <- rain_water_sump_pump2 %>%
  mutate(Value = ifelse(Value > 200, 200, Value))
rain_water2 <- rain_water2 %>%
  filter(Value > -100)
rain_water2 <- rain_water2 %>%
  filter(Value < 100)

sump_pump_stats_1 <- rain_water_sump_pump1 %>%
  group_by(date) %>%
  summarize(count = n(),
            min_watts = min(Value),
            mean_watts = mean(Value),
            max_watts = max(Value))
sump_pump_stats_2 <- rain_water_sump_pump2 %>%
  group_by(date) %>%
  summarize(count = n(),
            min_watts = min(Value),
            mean_watts = mean(Value),
            max_watts = max(Value))
rain_water_stats_1 <- rain_water1 %>%
  group_by(date) %>%
  summarize(sum_gallons = sum(Value))
rain_water_stats_2 <- rain_water2 %>%
  group_by(date) %>%
  summarize(sum_gallons = sum(Value))

sump_pump_stats_all <- rbind(sump_pump_stats_1, sump_pump_stats_2)
rain_water_stats_all <- rbind(rain_water_stats_1, rain_water_stats_2)

final_rain_water <- sump_pump_stats_all %>%
  left_join(rain_water_stats_all, by = c("date" = "date")) %>%
  filter(!is.na(sum_gallons))

final_rain_water <- final_rain_water %>%
  arrange(desc(sum_gallons)) %>%
  mutate(rank = row_number(),
         sum_gallons = sum_gallons / 252)

final_rain_water <- final_rain_water %>% 
  mutate_at(vars(mean_watts, sum_gallons), list(~ round(., 2)))

rain_cor <- final_rain_water %>%
  filter(sum_gallons > 0.1)

cor(rain_cor$mean_watts, rain_cor$sum_gallons) #-0.06

rain_tab_data <- final_rain_water %>%
  select(rank, date, mean_watts, sum_gallons) %>%
  filter(rank <= 15)

write.csv(rain_tab_data, "rain_tab_data.csv")

rain_gt <- rain_tab_data %>% 
  gt() %>% 
  text_transform(
    locations = cells_body(vars(drop1, drop2, drop3)),
    fn = function(x){
      web_image(
        url = x,
        height = px(25)
      )
    }
  ) %>% 
  cols_label(
    rank = "Rank",
    date = "Date",
    mean_watts = "Sump Pump Watts",
    sum_gallons = "Rain Water Gallons",
    drop1 = "",
    drop2 = "",
    drop3 = "") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = vars(rank)
    )
  ) %>% 
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  data_color(
    columns = vars(mean_watts),
    colors = scales::col_numeric(
      palette = c(
        "white", "orange", "red"),
      domain = NULL)
  ) %>%
  data_color(
    columns = vars(sum_gallons),
    colors = scales::col_numeric(
      palette = c(
        "dodgerblue", "dodgerblue2", "dodgerblue4"),
      domain = NULL)
  ) %>%
  tab_header(
    title = "How Rain Water Collected Correlates With Sump Pump Watts",
    subtitle = "Correlation coefficient of 0.00, top 20 days of total rain water selected due to outliers"
  ) %>%
  tab_options(
    column_labels.background.color = "white",
    column_labels.font.weight = "bold",
    table.border.top.width = px(3),
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    table.border.bottom.width = px(3),
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    column_labels.border.bottom.width = px(3),
    column_labels.border.bottom.color = "black",
    data_row.padding = px(3),
    source_notes.font.size = 12,
    table.font.size = 16,
    heading.align = "middle") %>%
  opt_table_font(
    font = list(
      default_fonts()
    )
  ) 
rain_gt
gtsave(rain_gt, "rain_gt.png")









