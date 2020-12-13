source("header.R")

sbf_set_sub("input")

sbf_load_datas()

sbf_set_sub("dump")

deerlifestage %<>% 
  filter(DeerLifeStage != "Unknown")

deersex %<>% 
  filter(DeerSex != "Unknown")

huntingtype %<>%
  mutate(HuntingType = if_else(str_detect(HuntingType, "Aerial"), "Helicopter", HuntingType),
         HuntingType = str_replace(HuntingType, "Shoreline", "Boat")) %>%
  distinct()

lead_hunter <- huntingteam %>%
  filter(HuntLead) %>%
  select(HuntingEventNumber, LeadHunter = Hunter)

dogs <- huntingteam %>%
  filter(str_detect(Hunter, "Dog")) %>%
  group_by(HuntingEventNumber) %>%
  summarise(Dogs = n(), .groups = "keep") %>%
  ungroup()

boats <- huntingteam %>%
  filter(str_detect(Hunter, "Boat")) %>%
  group_by(HuntingEventNumber) %>%
  summarise(Boats = n(), .groups = "keep") %>%
  ungroup()

huntingteam %<>%
  filter(!str_detect(Hunter, "Dog"),
         !str_detect(Hunter, "Boat"))

hunters <- huntingteam %>%
  group_by(HuntingEventNumber) %>%
  summarise(Hunters = n(), .groups = "keep") %>%
  ungroup()

huntingteam %<>%
  filter(!str_detect(Hunter, "Hunter")) %>%
  select(-HuntLead)

event %<>%
  mutate(GridSearch = str_detect(HuntingType, "Grid"),
         HuntingType = if_else(str_detect(HuntingType, "Aerial"), "Helicopter", HuntingType),
         HuntingType = str_replace(HuntingType, "Shoreline", "Boat"),
         Helicopters = if_else(HuntingType == "Helicopter", 1L, 0L)) %>%
  rename(PrimaryHuntingType = HuntingType) %>%
  left_join(lead_hunter, by = "HuntingEventNumber") %>%
  left_join(dogs, by = "HuntingEventNumber") %>%
  left_join(boats, by = "HuntingEventNumber") %>%
  left_join(hunters, by = "HuntingEventNumber") %>%
  replace_na(list(Dogs = 0L, Boats = 0L, OpportunisticHunting = FALSE)) %>%
  mutate(Dogs = if_else(str_detect(PrimaryHuntingType, "Dog") & Dogs == 0L, 1L, Dogs),
         Boats = if_else(str_detect(PrimaryHuntingType, "Boat") & Boats == 0L, 1L, Boats)) %>%
  select(-TeamHunting, -ShorelineWithDog, -HuntingPhase, -HuntingTimeCalculated) %>%
  select(HuntingEventNumber, PrimaryHuntingType, Island, LeadHunter, BaitStationID,
         DateTimeOutingStart, DateTimeOutingEnd, Dogs, Hunters, Boats, Helicopters, 
         OpportunisticHunting, GridSearch, CommentEvent, everything())

rm(dogs, lead_hunter, boats, hunters)

age %<>%
  select(-Island, -HuntEvent, -DateTimeAge, -Sex) %>%
  filter(!ToothID %in% c(14, 17))

encounter %<>%
  poisspatial::ps_activate_sfc() %>%
  poisspatial::ps_sfc_to_coords() %>%
  mutate(DeerLifeStage = if_else(DeerLifeStage == "Unknown", NA_character_, DeerLifeStage),
         DeerSex = if_else(DeerLifeStage == "Unknown", NA_character_, DeerSex)) %>%
  rename(SampleID = DNASampleCode) %>%
  left_join(shorelinekillsubtype, by = "ShorelineKillSubtype") %>%
  mutate(CommentEncounter = Description) %>%
  select(-ShorelineKillSubtype, -Description) %>%
  left_join(age, by = "SampleID") %>%
  select(HuntingEventNumber, EncounterID, DateTimeEncounter, Hunter, DeerStatus, DeerLifeStage, DeerSex, SampleID, ToothID, Age,
         X, Y, CommentEncounter, everything())

rm(shorelinekillsubtype, age)

sbf_save_table(island, report = FALSE)
sbf_save_table(deerstatus, report = FALSE)
sbf_save_table(deersex, report = FALSE)
sbf_save_table(deerlifestage, report = FALSE)
sbf_save_table(huntingtype, report = FALSE)
sbf_save_table(hunter, report = FALSE)

sbf_save_table(event, report = FALSE)
sbf_save_table(encounter, report = FALSE)
sbf_save_table(huntingteam, report = FALSE)

# add leadhunter to event
# add DateTime to huntingteam..