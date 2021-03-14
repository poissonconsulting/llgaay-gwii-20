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
         dtt_year(DateTimeOutingStart) == 2017) %>%
  mutate(Day = dtt_date(DateTimeOutingStart),
         Island = as.character(Island),
         Island = str_replace(Island, "\\s+Island$", ""),
         Island = factor(Island, levels = c("Ramsay", "Murchison", "House")),
         Type = as.character(Type),
         Type = if_else(GridSearch, "Miscellaneous", Type),
         Type = if_else(OpportunisticHunting, "Miscellaneous", Type),
         Type = if_else(Type %in% c("Line Push", "Walking"), "Miscellaneous", Type),
         Type = factor(Type, levels = c("Bait Station", "Boat", "Helicopter", "Indicator Dog", "Bailing Dog", "Miscellaneous")),
         DensityDependent = Type %in% c("Bait Station", "Helicopter", "Boat")) %>%
  select(HuntingEventNumber, Island, Area, Day, Type, DensityDependent, Hours, HourlyRate, Deer)

sbf_set_sub("rate")
sbf_save_data(data)
