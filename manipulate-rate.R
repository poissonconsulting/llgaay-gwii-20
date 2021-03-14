source("header.R")

sbf_set_sub("clean")
sbf_load_datas()

encounter %<>%
  as_tibble() %>%
  group_by(HuntingEventNumber, Status) %>%
  summarise(Deer = n(), .groups = "keep") %>%
  ungroup() %>%
  pivot_wider(names_from = "Status", values_from = "Deer")

data <- event %>% 
  left_join(encounter, by = "HuntingEventNumber") %>% 
  mutate(Dayte = dtt_dayte(DateTimeOutingStart),
         Year = dtt_year(DateTimeOutingStart),
         Annual = factor(Year)) %>%
  select(HuntingEventNumber, Island, Year, Annual, Dayte, Type, OpportunisticHunting, 
         GridSearch, Hours, HourlyRate, Deer = Killed)

sbf_set_sub("rate")
sbf_save_data(data)

data %<>% 
  mutate(Rate = Deer / Hours)

gp <- ggplot(data = data, aes(x = Dayte, y = Rate)) +
  facet_grid(Island ~ Year) +
  geom_point()

sbf_open_window()
sbf_print(gp)

