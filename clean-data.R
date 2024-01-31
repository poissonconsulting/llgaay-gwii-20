source("header.R")

sbf_set_sub("read")
sbf_load_datas()

sbf_save_table(rename(island, ScentTrials = Detections, EffectiveCoverage = ObsEff), x_name = "island", caption = "The number of scent trail detections by dogs and the effective coverage by island.", sub = "rate")

sbf_save_table(costs, caption = "The estimated hourly costs (in $) by crew member and equipment.", sub = "costs")

detection %<>%
  filter(Island == "Ramsay") %>%
  mutate(DetectionDate = dtt_date(paste(Year, Month, Day, sep = "-")),
         across(c(StartDeploy, `End Deploy`), mdy),
         Sex = case_when(
           Sex == "M" ~ "Male",
           Sex == "F" ~ "Female",
           TRUE ~ NA_character_)) %>%
  check_data(values = list(Count = c(1, 1), Island = c("Ramsay", "Ramsay"), 
                           Species = c("Deer", "Deer"))) %>%
  identity() %>%
  select(Camera, StartDeploy, EndDeploy = `End Deploy`, DetectionDate, Sex, Comments = Comment)

camera %<>%
  filter(Island == "Ramsay",
         str_detect(Code, "^RaBSC\\d{1,2}$")) %>%
  ps_coords_to_sfc(c("Longdd", "Latdd"))

helicrew <- costs %>%
  rename(Method = Type) %>%
  filter(Method %in% c("HeliHunter", "HeliPlusOperator")) %>%
  use_series("HourlyRate") %>%
  sum()

costs %<>%
  rename(Method = Type) %>%
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

islands %<>%
  mutate(Area = Area / 100)

event %>%
  filter(Island %in% c("Ramsay Island", "Murchison Island", "Hotspring Island")) %>%
  filter(!OpportunisticHunting, !GridSearch) %>%
  filter(!PrimaryHuntingType %in% c("Walking", "Line Push")) %>%
  count(PrimaryHuntingType, Dogs, Boats, Helicopters, OpportunisticHunting, GridSearch) %>%
  arrange(PrimaryHuntingType, Dogs, Boats, Helicopters)

event %<>%
  rename(Method = PrimaryHuntingType) %>%
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
  across(c(LeadHunter, Method), factor),
  across(c(DateTimeOutingStart, DateTimeOutingEnd), dtt_date_time),
  across(c(Dogs, Hunters, Boats, Helicopters), as.integer),
  Hours = as.numeric(difftime(DateTimeOutingEnd, DateTimeOutingStart, units = "hours")),
  HunterRate = case_when(
    Method == "Bait Station" ~ costs$HourlyRate[costs$Method == "BaitHunterAvg"],
    Method %in% c("Boat", "Walking") ~ costs$HourlyRate[costs$Method == "HunterAvg"],
    Method == "Helicopter" ~ costs$HourlyRate[costs$Method == "HeliHunter"],
    Method %in% c("Bailing Dog","Indicator Dog","Line Push") ~ costs$HourlyRate[costs$Method =="DogHunter"],
    TRUE ~ NA_real_),
  HourlyRate = Hunters * HunterRate,
  HourlyRate = HourlyRate + Dogs * costs$HourlyRate[costs$Method=="Dog"],
  HourlyRate = HourlyRate + Helicopters * costs$HourlyRate[costs$Method=="HeliPlusOperator"],
  HourlyRate = HourlyRate + Boats * costs$HourlyRate[costs$Method=="BoatPlusOperator"],
  Cost = Hours * HourlyRate)

sbf_set_sub("clean", rm = TRUE)
sbf_save_datas()
