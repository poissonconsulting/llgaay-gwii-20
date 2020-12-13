source("header.R")

sbf_set_sub("read")
sbf_load_datas()

encounter %<>% 
  ps_coords_to_sfc(c("Longitude", "Latitude"), crs = 4269)

x <- left_join(encounter, select(event, Island, HuntingEventNumber), by = "HuntingEventNumber") %>%
  group_split(Island)

islands <- vapply(x, function(x) x$Island[1], "")

names(x) <- islands

mapview(x)
