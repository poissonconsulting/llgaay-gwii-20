source("header.R")

#merging of dataframes or usage happens in tidy
sbf_set_sub("clean")
sbf_load_datas()

data <- inner_join(encounter, event, by = "HuntingEventNumber")

event %>% select (HuntingEventNumber, Type, Island,LeadHunter, Dogs, Hunters, Boats, Helicopters,DateTimeOutingStart, DateTimeOutingEnd, DiffTime)
effdata <- left_join(event, huntingteam, by = "HuntingEventNumber")

sbf_set_sub("tidy", rm = TRUE)
sbf_save_datas()
