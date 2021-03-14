source("header.R")

#merging of dataframes or usage happens in tidy
sbf_set_sub("clean")
sbf_load_datas()

effort <- left_join(event, huntingteam, by = "HuntingEventNumber")

data <- inner_join(encounter, event, by = "HuntingEventNumber")

event %>% select (HuntingEventNumber, Type, Island,LeadHunter, Dogs, Hunters, Boats, Helicopters,DateTimeOutingStart, DateTimeOutingEnd, DiffTime)

effdata %<>% mutate(HourlyRate=Dogs*costs$HourlyRate[costs$Type=="Dog"],
                    HourlyRate=Helicopters*costs$HourlyRate[costs$Type=="HeliPlusOperator"],
                    HourlyRate=Boats*costs$HourlyRate[costs$Type=="BoatPlusOperator"],
                    HourlyRate = case_when(
                      Type == "Bait Station" ~ HourlyRate + Hunters * costs$HourlyRate[costs$Type == "BaitHunterAvg"],
                      Type %in% c("Boat", "Walking") ~ HourlyRate + Hunters *costs$HourlyRate[costs$Type == "HunterAvg"],
                      Type == "Helicopter"~ HourlyRate +Hunters *costs$HourlyRate[costs$Type == "HeliHunter"],
                      Type %in% c("Bailing Dog","Indicator Dog","Line Push") ~ HourlyRate + Hunters * costs$HourlyRate[costs$Type =="DogHunter"],
                     TRUE ~ 1000000))

effdata$EventCost<- effdata$HourlyRate *effdata$DiffTimeHr


sbf_set_sub("tidy", rm = TRUE)
sbf_save_datas()
