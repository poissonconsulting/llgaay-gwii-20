source("header.R")

sbf_set_sub("read")
sbf_load_datas()

encounter %<>% 
  ps_coords_to_sfc(c("Longitude", "Latitude"), crs = 4269)

encounter %<>%
  mutate(DeerLifeStage = factor(DeerLifeStage, levels = c("Juvenile", "Subadult", "Adult"))) %>%
  rename(LifeStage = DeerLifeStage)

sbf_set_sub("clean", rm = TRUE)
sbf_save_datas()


