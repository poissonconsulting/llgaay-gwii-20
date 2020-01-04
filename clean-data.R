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
        mutate (Sex=toupper(Sex))  %>%
  select(ToothID, SampleID, Island, DateTime, HuntEvent, Sex)

bailingeffortdata %<>%
  mutate(StartHour=HuntingStartHour, StartMin=HuntingStartMinute, StopHour=HuntingStopHour, StopMin=HuntingStopMinute, Island=IslandName, 
         StartDateTime =ISOdatetime (year=HuntingStartYear, month=HuntingStartMonth, day=HuntingStartDay, StartHour,StartMin, sec=0 , tz="PST8PDT"), 
         StopDateTime =ISOdatetime (year=HuntingStopYear, month=HuntingStopMonth, day=HuntingStopDay, StopHour,StopMin, sec=0 , tz="PST8PDT")) %>%
  select(HuntingEventNumber, HuntingType, Island, HunterName, StartDateTime, StopDateTime,Comments, TeamHunting, HuntingEventHuntingTime, Totaltimeoftrackoverisland,TrackLength=`Length(m)`)

bailingeffortdata %<>% fill(StartDateTime, StopDateTime, Island, HuntingType)

killobsdata %<>%
        mutate(Island=IslandName, Hunter=HunterName, 
         DateTime =ISOdatetime (Year, Month, Day, EncounterHour,EncounterMinute, sec=0 , tz="PST8PDT"))  %>%
        mutate(Latitude=as.numeric(Latitude), Longitude=as.numeric(Longitude)) %>%
  select(HuntingEventNumber, Island, Hunter, Latitude, Longitude, DateTime, DeerStatus, DeerLifestage, DeerSex, DNASample, DNASampleCode, Comments, Count, BailKill, Type, SubTypeKillTechnique)

eventdata %<>%
      mutate(Island=IslandName, Hunter=HunterName, TrackLength=`Length(m)`, Hours=`Hours(Calc)`,
      StartDateTime=ISOdatetime(HuntingStartYear,HuntingStartMonth, HuntingStartDay, HuntingStartHour, HuntingStartMinute, sec=0, tz="PST8PDT"),
      EndDateTime = ISOdatetime(HuntingStopYear, HuntingStopMonth, HuntingStopDay,HuntingStopHour, HuntingStopMinute, sec=0, tz="PST8PDT")) %>%
      select(HuntingEventNumber,HuntingType,Island, Hunter,BaitStationID, StartDateTime, EndDateTime,Comments, OpportunisticHunting, TeamHunting, ShorelineWithDog, HuntingPhase,HuntingEventHuntingTime, Totaltimeoftrackoverisland, StartMin, EndMin, TotalMin, TimeCalc, TrackFileMissingorCorruptororIncomplete,TrackLength, Hours)


sbf_set_sub("clean", rm = TRUE)

sbf_save_datas()
