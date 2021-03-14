source("header.R")

sbf_set_sub("clean")
sbf_load_datas()

check_data(event, values = list(
  HuntingEventNumber = "",
  Dogs = 0:7,
  Hours = c(1, 14),
  Island = factor("")
))

check_data(encounter, values = list(
  HuntingEventNumber = "",
  EncounterID = "",
  Status = factor("")
))

#check all primary key values for each of three datafiles is unique
check_key(event, key = "HuntingEventNumber")
check_key(encounter, key = c("HuntingEventNumber", "EncounterID"))
check_key(huntingteam, key = c("HuntingEventNumber", "Hunter"))

chk_join(encounter, event, by = "HuntingEventNumber")

#Mapview no longer works. Have to backconvert island to a character for funtion, but a NA appears, really confusing about what vapply needs and why it sometimes
#works and soemtimes doesn't... need help debugging. Could reorder again in run=all .
#event$Island<-as.character(event$Island, rm.na=TRUE)
x <- left_join(encounter, select(event, Island, HuntingEventNumber), by = "HuntingEventNumber") %>%
  group_split(Island)
islands <- vapply(x, function(x) as.character(x$Island[1]), "")
names(x) <- islands
mapview(x)

#no saving because this is just a checkpoint; clean and tidy data get saved