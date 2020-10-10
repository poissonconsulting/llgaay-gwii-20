source("header.R")

sbf_set_sub("input")

sbf_load_datas()

missing_age <- anti_join(age, encounter, by = c("SampleID" = "DNASampleCode"))

sbf_save_table(missing_age)

message("check 'Dog Hot Spot Hunting' is Indicator Dog")
event %<>% 
  filter(Island == "Ramsay Island") %>%
   mutate(HuntingType = if_else(!is.na(OpportunisticHunting) & OpportunisticHunting, "Opportunistic", HuntingType)) %>%
  select(HuntingEventNumber, HuntingType, DateTimeOutingStart, 
         HuntingTimeCalculated, BaitStationID) %>%
  mutate(BaitStationID = factor(BaitStationID),
         HuntingType = if_else(str_detect(HuntingType, "Aerial"), "Aerial", HuntingType),
         HuntingType = str_replace(HuntingType, "Walking", "Opportunistic"),
         HuntingType = str_replace(HuntingType, "Dog Hot Spot Hunting", "Indicator Dog"),
         HuntingType = factor(HuntingType, levels = c("Bait Station", "Aerial", "Shoreline", "Bailing Dog", "Indicator Dog", "Opportunistic")))

# set hunting types as factor

huntingteam %<>% 
  semi_join(event, by = "HuntingEventNumber") %>%
  select(HuntingEventNumber, Hunter, TrackLength) %>%
  mutate(Hunter = factor(Hunter))

encounter %<>% 
  semi_join(event, by = "HuntingEventNumber") %>%
  mutate(Culled = DeerStatus == "Killed") %>%
  select(EncounterID, HuntingEventNumber, DateTimeEncounter, Hunter, Culled,
         LifeStage = DeerLifeStage, Sex = DeerSex, DNASampleCode, geometry) %>%
  mutate(Hunter = factor(Hunter, levels = levels(huntingteam$Hunter)),
         Sex = if_else(Sex == "Unknown", NA_character_, Sex),
         Sex = factor(Sex),
         LifeStage = if_else(LifeStage == "Unknown", NA_character_, LifeStage),
         LifeStage = factor(LifeStage, levels = c("Juvenile", "Subadult", "Adult")))

# check consistency age 
age %<>% 
  filter(!is.na(Age)) %>%
  semi_join(encounter, by = c("SampleID" = "DNASampleCode")) %>%
  select(SampleID, Age)

check_key(age, "SampleID")

sbf_set_sub("clean", rm = TRUE)

sbf_save_data(event)
sbf_save_data(encounter)
sbf_save_data(huntingteam)
sbf_save_data(age)
