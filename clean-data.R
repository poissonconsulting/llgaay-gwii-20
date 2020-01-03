source("header.R")

sbf_set_sub("read")

sbf_load_datas()

agedata %<>% 
  filter(Age != "X") %>%
  select(ToothID = `Tooth ID`, Age) %>%
  mutate(ToothID = as.integer(ToothID), 
         Age = as.integer(Age))

agesourcedata %<>% 
  mutate(Hour = substr(Time, 1, 2),
         Minute = substr(Time, 3, 4),
         DateTime = ISOdatetime(Year, Month, Day, Hour, Minute, 0L, tz = "PST8PDT"),
         ToothID = as.integer(ToothID)) %>%
  select(ToothID, SampleID, Island, DateTime, HuntEvent, Sex)

agesourcedata %<>%
  mutate(Sex=toupper(Sex))

sbf_set_sub("clean", rm = TRUE)

sbf_save_datas()
