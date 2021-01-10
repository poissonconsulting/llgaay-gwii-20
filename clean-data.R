source("header.R")

sbf_set_sub("read")
sbf_load_datas()

encounter %<>%
  ps_coords_to_sfc(c("Longitude", "Latitude"), crs = 4269)

encounter %<>%
  mutate(DeerLifeStage = factor(DeerLifeStage, levels = c("Juvenile", "Subadult", "Adult"))) %>%
  rename(LifeStage = DeerLifeStage)

encounter %<>%
  mutate(DateTimeEncounter = dtt_date_time(DateTimeEncounter))

encounter %<>%
  mutate(Hunter = factor (Hunter))

encounter %<>%
  mutate(DeerStatus = factor(DeerStatus, levels = c("Observed", "Killed"))) %>%
  rename(Status = DeerStatus)

encounter %<>%
  mutate(DeerSex = factor(DeerSex, levels = c("Female", "Male", "Unknown", "NA"))) %>%
  rename(Sex = DeerSex)

event %<>%
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
  ))


event %<>%
  mutate(LeadHunter = factor (LeadHunter))

event %<>%
  mutate(PrimaryHuntingType = factor (PrimaryHuntingType)) %<>%
  rename(Type = PrimaryHuntingType)


sbf_set_sub("clean", rm = TRUE)
sbf_save_datas()
