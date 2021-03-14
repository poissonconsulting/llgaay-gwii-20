source("header.R")

sbf_set_sub("read")
sbf_load_datas()

helicrew <- costs %>%
  filter(Type %in% c("HeliHunter", "HeliPlusOperator")) %>%
  use_series("HourlyRate") %>%
  sum()

costs %<>%
  mutate(HourlyRate = HourlyRate / helicrew)

encounter %<>%
  rename(LifeStage = DeerLifeStage,
         Status = DeerStatus,
         Sex = DeerSex) %>%
  mutate(LifeStage = factor(LifeStage, levels = c("Juvenile", "Subadult", "Adult")),
         DateTimeEncounter = dtt_date_time(DateTimeEncounter),
         Hunter = factor(Hunter),
         Status = factor(Status, levels = c("Observed", "Killed")),
         Sex = factor(Sex, levels = c("Female", "Male", "Unknown", "NA")),
         across(c(ToothID, Age), as.integer)) %>%
  ps_coords_to_sfc(c("Longitude", "Latitude"), crs = 4269)

event %<>%
  rename(Type = PrimaryHuntingType) %>%
  left_join(islands, by = "Island") %>%
  mutate(Island = factor(
    Island,
    levels = c(
      "Ramsay Island",
      "Murchison Island",
      "Faraday Island",
      "Bischof Islands",
      "Hotspring Island",
      "House Island",
      "Lyell Island",
      "Moresby Island",
      "Sgang Gwaay Island",
      "Langtry Island",
      "Howay Island"
    )
  ),
  across(c(LeadHunter, Type), factor),
  across(c(DateTimeOutingStart, DateTimeOutingEnd), dtt_date_time),
  across(c(Dogs, Hunters, Boats, Helicopters), as.integer),
  Hours = as.numeric(difftime(DateTimeOutingEnd, DateTimeOutingStart, units = "hours")),
  HunterRate = case_when(
    Type == "Bait Station" ~ costs$HourlyRate[costs$Type == "BaitHunterAvg"],
    Type %in% c("Boat", "Walking") ~ costs$HourlyRate[costs$Type == "HunterAvg"],
    Type == "Helicopter" ~ costs$HourlyRate[costs$Type == "HeliHunter"],
    Type %in% c("Bailing Dog","Indicator Dog","Line Push") ~ costs$HourlyRate[costs$Type =="DogHunter"],
    TRUE ~ NA_real_),
  HourlyRate = Hunters * HunterRate,
  HourlyRate = HourlyRate + Dogs * costs$HourlyRate[costs$Type=="Dog"],
  HourlyRate = HourlyRate + Helicopters * costs$HourlyRate[costs$Type=="HeliPlusOperator"],
  HourlyRate = HourlyRate + Boats * costs$HourlyRate[costs$Type=="BoatPlusOperator"],
  Cost = Hours * HourlyRate)

sbf_set_sub("clean", rm = TRUE)
sbf_save_datas()
