source("header.R")

sbf_set_sub("clean")
sbf_load_datas()

data <- encounter %>%
  as_tibble() %>%
  filter(Status == "Killed") %>%
  group_by(HuntingEventNumber) %>%
  summarise(Deer = n(), .groups = "keep") %>%
  ungroup() %>% 
  right_join(event, by = "HuntingEventNumber") %>%
  replace_na(list(Deer = 0L)) %>%
  filter(Island %in% c("Ramsay Island", "Murchison Island", "House Island"),
         dtt_year(DateTimeOutingStart) == 2017,
         !GridSearch,
         !(OpportunisticHunting & Deer == 0)) %>%
  mutate(Day = dtt_date(DateTimeOutingStart),
         Island = as.character(Island),
         Island = str_replace(Island, "\\s+Island$", ""),
         Island = factor(Island, levels = c("Ramsay", "Murchison", "House")),
         Type = as.character(Type),
         Type = if_else(OpportunisticHunting, "Opportunistic", Type),
         Type = factor(Type)) %>%
  select(HuntingEventNumber, Island, Area, Day, Type, Hours, HourlyRate, Deer)

sbf_set_sub("rate")
sbf_save_data(data)

gp <- ggplot(data = data, aes(x = Day, y = Deer)) +
  facet_wrap(~Type, scales = "free_y") +
  geom_point(aes(color = Island), alpha = 2/3,
             position = position_jitter(height = 0.1)) +
  expand_limits(y = 0) +
  scale_color_manual(values = c("black", "blue", "red")) +
  theme(legend.position = "bottom")

sbf_open_window()
sbf_print(gp)

gp <- gp + aes(x = Day, y = Deer / Hours)

sbf_open_window()
sbf_print(gp)

gp <- gp + aes(x = Day, y = Deer / (Hours * HourlyRate) * 1000)

sbf_open_window()
sbf_print(gp)

