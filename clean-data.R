source("header.R")

sbf_set_sub("input")

sbf_load_datas()

event %<>% 
  filter(Island == "Ramsay") %>%
  select(HuntingEventNumber, HuntingType, DateTimeOutingStart, HuntingTimeCalculated) %>%
  

sbf_set_sub("clean", rm = TRUE)

sbf_save_datas()
