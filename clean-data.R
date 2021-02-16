source("header.R")

#changing of single dataframe happens in clean

sbf_set_sub("read")
sbf_load_datas()

encounter %<>%
  ps_coords_to_sfc(c("Longitude", "Latitude"), crs = 4269)

encounter %<>%
  mutate(DeerLifeStage = factor(DeerLifeStage, levels = c("Juvenile", "Subadult", "Adult"))) %>%
  rename(LifeStage = DeerLifeStage) %>%
  mutate(DateTimeEncounter = dtt_date_time(DateTimeEncounter)) %>%
  mutate(Hunter = factor (Hunter)) %>%
  mutate(DeerStatus = factor(DeerStatus, levels = c("Observed", "Killed"))) %>%
  rename(Status = DeerStatus) %>%
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
  )) %>%
  mutate(LeadHunter = factor (LeadHunter)) %>%
  mutate(PrimaryHuntingType = factor (PrimaryHuntingType)) %<>%
  mutate(DateTimeOutingStart = dtt_date_time(DateTimeOutingStart)) %>%
  mutate(DateTimeOutingEnd = dtt_date_time(DateTimeOutingEnd)) %>%
  rename(Type = PrimaryHuntingType)

#getting time on the ground in event
event$DiffTime<-as.numeric(difftime(event$DateTimeOutingEnd, event$DateTimeOutingStart,units = c("mins")))

#costs - do i need it in here for it to carry through

sbf_set_sub("clean", rm = TRUE)
sbf_save_datas()
