source("header.R")

sbf_set_sub("read")
sbf_load_datas()

agedata %<>% 
  filter(Age != "X") %>%
  transmute(ToothID = as.integer(`Tooth ID`), 
         Age = as.integer(Age))

agesourcedata %<>% 
  mutate(Hour = substr(Time, 1, 2),
         Minute = substr(Time, 3, 4),
         DateTimeAge = ISOdatetime(Year, Month, Day, Hour, Minute, 0L, tz = tz_data),
         DateTimeAge = with_tz(DateTimeAge, tz = tz_analysis),
         ToothID = as.integer(ToothID)) %>%
  mutate (Sex = toupper(Sex))  %>%
  select(ToothID, SampleID, Island, DateTimeAge, HuntEvent, Sex)

chk_subset(agesourcedata$Sex, c("M", "F", NA))
check_join(agedata, agesourcedata, "ToothID")

age <- bind_rows(agedata, agesourcedata)

bailingeffortdata <- bailingeffortdata %>%
  mutate(Island = IslandName, 
         Hunter = HunterName,
         TrackLength = `Length(m)`,
         DateTimeStart = ISOdatetime(HuntingStartYear, HuntingStartMonth, 
                                            HuntingStartDay, HuntingStartHour,
                                            HuntingStartMinute, 0L, tz = tz_data), 
         DateTimeEnd = ISOdatetime(HuntingStopYear, HuntingStopMonth, 
                                          HuntingStopDay, HuntingStopHour, 
                                          HuntingStopMinute, 0L, tz = tz_data))

eventdata <- eventdata %>%
  mutate(Island = IslandName, 
         Hunter = HunterName,
         TrackLength = `Length(m)`,
         DateTimeStart = ISOdatetime(HuntingStartYear, HuntingStartMonth, 
                                     HuntingStartDay, HuntingStartHour,
                                     HuntingStartMinute, 0L, tz = tz_data), 
         DateTimeEnd = ISOdatetime(HuntingStopYear, HuntingStopMonth, 
                                   HuntingStopDay, HuntingStopHour, 
                                   HuntingStopMinute, 0L, tz = tz_data),
         OpportunisticHunting = if_else(tolower(OpportunisticHunting) == "yes",
                                        TRUE, FALSE),
         TeamHunting = if_else(tolower(TeamHunting) == "yes",
                                        TRUE, FALSE),
         ShorelineWithDog = if_else(tolower(ShorelineWithDog) == "yes",
                               TRUE, FALSE),
         TrackError = if_else(TrackFileMissingorCorruptororIncomplete == 1,
                              TRUE, FALSE, missing = FALSE))

message("if TrackFileMissingorCorruptorIncomplete == 1, set to TRUE else FALSE")
check_join(bailingeffortdata %>% filter(!is.na(HuntingEventNumber)), eventdata, "HuntingEventNumber")

message("should check that values for Island, Type, Hunter, Start, End are identical in both tables")

outing <- eventdata %>%
  select(OutingID = HuntingEventNumber,
         DateTimeOutingStart = DateTimeStart,
         DateTimeOutingEnd = DateTimeEnd,
         Island,
         Hunter,
         HuntingType,
         OpportunisticHunting,
         TeamHunting,
         ShorelineWithDog,
         HuntingPhase,
         HuntingTime = HuntingEventHuntingTime,
         TrackOverIslandTime = Totaltimeoftrackoverisland,
         HuntingTimeCalculated = TimeCalc,
         TrackFileError = TrackFileMissingorCorruptororIncomplete,
         CommentHunting = Comments)

### add some missing info from bailing effort table
outing <- bailingeffortdata %>%
  select(OutingID = HuntingEventNumber,
         NumberOfDogs = Dogs,
         NumberOfHunters = Hunters,
         NumberOfBoats = Boats,
         Heli) %>%
  group_by(OutingID) %>%
  slice(1) %>%
  mutate(Heli = if_else(Heli == 1, TRUE, FALSE)) %>%
  right_join(outing, "OutingID")

### add faraday event data
outing <- faradayevent %>%
  transmute(OutingID = HuntingEventNumber,
            HuntingType,
            Island = paste(IslandName, "Island"), 
            Hunter = LeadHunterName,
            NumberOfHunters,
            NumberOfDogs,
            NumberOfBoats,
            DateTimeOutingStart = ISOdatetime(HuntingStartYear, HuntingStartMonth, 
                                        HuntingStartDay, HuntingStartHour,
                                        HuntingStartMinute, 0L, tz = tz_data), 
            DateTimeOutingEnd = ISOdatetime(HuntingStopYear, HuntingStopMonth, 
                                      HuntingStopDay, HuntingStopHour, 
                                      HuntingStopMinute, 0L, tz = tz_data)) %>%
  bind_rows(outing)

message("is NumberOfDogs etc. same as Dogs, Hunters, etc. in eventdata table.
        why doesnt this match number of dogs in eventdata table?")
check_key(outing, "OutingID")

baitstation <- eventdata %>%
  filter(!is.na(BaitStationID), !(BaitStationID %in% c("Ra00", "RA00", "RAS",
                                                       "LYS", "NA", "MAS", "HAS"))) %>%
  select(OutingID = HuntingEventNumber,
         BaitStationID)

### confirmed that all baitstation OutingID had HuntingType == "Bait Station"
check_join(baitstation, outing, "OutingID")

message("make sure that keeping dogs to calculate effort in bailing")
message("do we need a separate bailingeffort table? with dogs, heli, ")
message("why does number of dogs in Dogs column not match Dog in HunterName column")

track <- bailingeffortdata %>%
  select(OutingID = HuntingEventNumber,
         Hunter, 
         TrackLength) %>%
  filter(!is.na(TrackLength))

track2 <- eventdata %>%
  select(OutingID = HuntingEventNumber, 
         Hunter, 
         TrackLength) %>%
  filter(!is.na(TrackLength))
  
track <- bind_rows(track, track2)

check_join(track, outing, "OutingID")
  
encounter <- killobsdata %>%
  transmute(OutingID = HuntingEventNumber,
            EncounterID = RecordNumber,
            DateTimeEncounter = ISOdatetime(Year, Month, Day, EncounterHour, 
                                            EncounterMinute, 0L, tz = tz_data),
            DateTimeEncounter = dtt_adjust_tz(DateTimeEncounter, tz = tz_analysis),
            Killed = if_else(tolower(DeerStatus) == "killed", TRUE, FALSE),
            DeerLifestage = if_else(tolower(DeerLifestage) == "unknown", NA_character_,
                                    DeerLifestage),
            DeerSex = if_else(tolower(DeerSex) == "unknown", NA_character_,
                              DeerSex),
            DNASampleCode = if_else(DNASampleCode == "NA", NA_character_, DNASampleCode),
            DNASample = if_else(tolower(DNASample) != "yes", FALSE, TRUE),
            Longitude,
            Latitude)

faradaykill <- faradaykill %>%
  transmute(OutingID = HuntingEventNumber,
            EncounterID = EncounterNumber,
            Longitude, 
            Latitude,
            DateTimeEncounter = ISOdatetime(EncounterYear, EncounterMonth, 
                                            EncounterDay, EncounterHour, 
                                            EncounterMinute, 0L, tz = tz_data),
            DateTimeEncounter = dtt_adjust_tz(DateTimeEncounter, tz = tz_analysis),
            ## all were killed
            Killed = TRUE,
            DeerLifestage = if_else(tolower(DeerLifestage) == "unknown", NA_character_,
                                    DeerLifestage),
            DeerSex = if_else(tolower(DeerSex) == "unknown", NA_character_,
                              DeerSex),
            DNASampleCode = if_else(DNASampleCode == "NA", NA_character_, DNASampleCode),
            DNASample = if_else(tolower(DNASample) != "yes", FALSE, TRUE),
            Comments)

message("in faraday data why are there cases of `No` for DNASample but also a DNASampleCode")

### deal with coordinates
message("There are two coords with messages: Wpt 006 on Yo Dang and 
Wpt 007 on Yo Dang...does this mean anything to you? removing for now")
encounter$Longitude[encounter$Longitude == "NA"] <- NA_character_
encounter$Longitude[encounter$Longitude == "WPT 006 on Yo Dang"] <- NA_character_
encounter$Longitude[encounter$Longitude == "WPT 007 on Yo Dang"] <- NA_character_
encounter$Latitude[encounter$Latitude == "NA"] <- NA_character_

encounter$Latitude %<>% as.numeric()
encounter$Longitude %<>% as.numeric()

faradaykill$Longitude <- if_else(abs(faradaykill$Longitude) > 200, faradaykill$Longitude/10^5, faradaykill$Longitude)

### fix when latitude and longitude reversed
encounter <- encounter %>%
  mutate(Latitude2 = if_else(abs(Latitude) > 100, Longitude, Latitude),
         Longitude2 = if_else(abs(Longitude) > 100, Longitude, Latitude),
         Longitude2 = if_else(Longitude2 > 0, -Longitude2, Longitude2),
         Longitude = Longitude2,
         Latitude = Latitude2,
         Longitude2 = NULL,
         Latitude2 = NULL)

encounter <- bind_rows(encounter, faradaykill)

encounter %<>% ps_coords_to_sfc(c("Longitude", "Latitude"), crs = 4326)

### check that coords make sense
mapview::mapview(encounter %>% filter(str_detect(EncounterID, "F")))

message("need to fix coords not on an island")

missing_sex <- filter(encounter, (DNASample) & is.na(DeerSex))
# there are no cases  where there is DNASampleCode but no sex info
chk_true(identical(nrow(missing_sex), 0L))

hunter <- tibble(Hunter = unique(outing$Hunter))
island <- tibble(Island = unique(outing$Island))
huntingtype <- tibble(HuntingType = unique(outing$HuntingType))

sbf_set_sub("prepare")
sbf_save_data(outing)
sbf_save_data(age)
sbf_save_data(encounter)
sbf_save_data(track)
sbf_save_data(hunter)
sbf_save_data(island)
sbf_save_data(huntingtype)

