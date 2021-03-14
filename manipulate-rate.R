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
  filter(Island %in% c("Ramsay Island", "Murchison Island", "Hotspring Island", "House Island"),
         dtt_year(DateTimeOutingStart) == 2017,
         !GridSearch,
         !(OpportunisticHunting & Deer == 0)) %>%
  mutate(Date = dtt_date(DateTimeOutingStart),
         Island = droplevels(Island),
         Type = as.character(Type),
         Type = if_else(OpportunisticHunting, "Opportunistic", Type),
         Type = factor(Type)) %>%
  select(HuntingEventNumber, Island, Date, Type, Hours, HourlyRate, Deer)

sbf_set_sub("rate")
sbf_save_data(data)

gp <- ggplot(data = data, aes(x = Date, y = Deer)) +
  facet_wrap(~Type, scales = "free_y") +
  geom_point(aes(color = Island)) +
  expand_limits(y = 0) +
  theme(legend.position = "bottom")

sbf_open_window()
sbf_print(gp)

gp <- gp + aes(x = Date, y = Deer / Hours)

sbf_open_window()
sbf_print(gp)

gp <- gp + aes(x = Date, y = Deer / (Hours * HourlyRate) * 1000)

sbf_open_window()
sbf_print(gp)

