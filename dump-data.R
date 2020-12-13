source("header.R")

sbf_set_sub("input")

sbf_load_datas()

sbf_set_sub("dump")

deerlifestage %<>% 
  filter(DeerLifeStage != "Unknown")

deersex %<>% 
  filter(DeerSex != "Unknown")

huntingtype %<>%
  mutate(HuntingType = if_else(str_detect(HuntingType, "Aerial"), "Aerial", HuntingType)) %>%
  distinct()

event %<>%
  mutate(GridSearch = str_detect(HuntingType, "Grid"),
         HuntingType = if_else(str_detect(HuntingType, "Aerial"), "Aerial", HuntingType)) %>%
  select(-TeamHunting, -ShorelineWithDog, -HuntingPhase)

unique(event$HuntingType)

encounter %<>%
  poisspatial::ps_activate_sfc() %>%
  poisspatial::ps_sfc_to_coords() %>%
  mutate(DeerLifeStage = if_else(DeerLifeStage == "Unknown", NA_character_, DeerLifeStage),
         DeerSex = if_else(DeerLifeStage == "Unknown", NA_character_, DeerSex)) %>%
  rename(SampleID = DNASampleCode) %>%
  left_join(shorelinekillsubtype, by = "ShorelineKillSubtype") %>%
  select(-ShorelineKillSubtype) %>%
  rename(ShoreLineKillSubtype = Description)

rm(shorelinekillsubtype)

age %<>%
  select(-Island, -HuntEvent, -DateTimeAge, -Sex) %>%
  filter(!ToothID %in% c(14, 17))

encounter %<>%
  left_join(age, by = "SampleID")

rm(age)

hunter %<>% 
  mutate(Hunter = str_replace(Hunter, "Norm Macdonald", "Helicopter"))

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