source("header.R")

sbf_set_sub("read")
sbf_load_datas()

########## Event Table ##########
# event table has key of HuntingEventNumber - hunting team and tracks moved to other tables
event <- eventdata %>%
  mutate(Island = IslandName, 
         TrackLength = `Length(m)`,
         DateTimeOutingStart = ISOdatetime(HuntingStartYear, HuntingStartMonth, 
                                           HuntingStartDay, HuntingStartHour,
                                           HuntingStartMinute, 0L, tz = tz_data), 
         DateTimeOutingEnd = ISOdatetime(HuntingStopYear, HuntingStopMonth, 
                                         HuntingStopDay, HuntingStopHour, 
                                         HuntingStopMinute, 0L, tz = tz_data),
         OpportunisticHunting = if_else(tolower(OpportunisticHunting) == "yes",
                                        TRUE, FALSE),
         TeamHunting = if_else(tolower(TeamHunting) == "yes",
                               TRUE, FALSE),
         ShorelineWithDog = if_else(tolower(ShorelineWithDog) == "yes",
                                    TRUE, FALSE),
         BaitStationID = if_else(BaitStationID %in% c("Ra00", "RA00", "RAS",
                                                      "LYS", "NA", "MAS", "HAS"), NA_character_, BaitStationID))

### add some missing info from bailing effort table
event <- bailingeffortdata %>%
  select(HuntingEventNumber,
         NumberOfDogs = Dogs,
         NumberOfHunters = Hunters,
         NumberOfBoats = Boats,
         Heli) %>%
  group_by(HuntingEventNumber) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(Heli = if_else(Heli == 1, TRUE, FALSE)) %>%
  right_join(event, "HuntingEventNumber")

event <- event %>%
  select(HuntingEventNumber,
         DateTimeOutingStart,
         DateTimeOutingEnd,
         Island,
         HuntingType,
         NumberOfDogs,
         NumberOfHunters,
         NumberOfBoats,
         OpportunisticHunting,
         TeamHunting,
         ShorelineWithDog,
         HuntingPhase,
         BaitStationID,
         HuntingTime = HuntingEventHuntingTime,
         TrackOverIslandTime = Totaltimeoftrackoverisland,
         HuntingTimeCalculated = TimeCalc,
         CommentEvent = Comments)

### add faraday events
event_faraday <- faradayevent %>%
  transmute(HuntingEventNumber,
            HuntingType,
            Island = paste(IslandName, "Island"), 
            NumberOfHunters,
            NumberOfDogs,
            NumberOfBoats,
            DateTimeOutingStart = ISOdatetime(HuntingStartYear, HuntingStartMonth, 
                                              HuntingStartDay, HuntingStartHour,
                                              HuntingStartMinute, 0L, tz = tz_data), 
            DateTimeOutingEnd = ISOdatetime(HuntingStopYear, HuntingStopMonth, 
                                            HuntingStopDay, HuntingStopHour, 
                                            HuntingStopMinute, 0L, tz = tz_data)) 

event <- bind_rows(event, event_faraday)

### check that all HuntingEventNumbers accounted for in event table
check_key(event, "HuntingEventNumber")
check_join(bailingeffortdata %>% filter(!is.na(HuntingEventNumber)), event, "HuntingEventNumber")
check_join(faradayevent, event, "HuntingEventNumber")

message("change aerial hotspot to aerial and add killsubtype")
########## event table issues ##########
# 1. What are the definitions of the all of the time columns? (not in metadata) which to keep?
# 2. Do we need to include NumberOfDogs, NumberOfBoats, etc.?
message("recreate hunting team from faraday event table (fill in names from kill table when possible), delete dogs, boats, hunters in bailing effort as doesnt seem to be correct")
message("for now were not gonna worry about filling out the team members for early events")
### shoreline until may 20th one shooter, after that might be more team members
# 3. Why is NumberOfDogs not the sum of the number of dogs in hunting team for bailingeffort events? how calculated?
# 4. For faradayevent we have the number of crew members, but only a single name
# (whereas bailingeffort we have names, etc.) - could recreate by naming Hunter1, 
# Hunter2 etc. and allowing user to recreate summary from huntingteam table
summary <- bailingeffortdata %>%
  group_by(HuntingEventNumber) %>%
  summarise(NumberOfDogs = sum(str_detect(tolower(HunterName), "dog")),
         NumberOfHunters = sum(!str_detect(tolower(HunterName), "dog|boat|helicopter|heli")),
         NumberOfBoats = sum(str_detect(tolower(HunterName), "boat")),
         Heli = if_else("helicopter" %in% tolower(HunterName), TRUE, FALSE)) %>%
  ungroup()

summary2 <- bailingeffortdata %>%
  select(HuntingEventNumber, Dogs, Hunters, Boats, Heli) %>%
  group_by(HuntingEventNumber) %>%
  slice(1) %>%
  ungroup()

########## Hunting team table (includes track and situations where multiple team members) ##########
# primary key HuntingEventNumber and Hunter
# TrackFileError has 1 or NA - turn NA into false for eventdata and bailingeffort
# TrackFileError for faradayevent is NA because no information
huntingteam_event <- eventdata %>%
  transmute(HuntingEventNumber,
         Hunter = HunterName,
         TrackFileError = TrackFileMissingorCorruptororIncomplete,
         TrackFileError = if_else(TrackFileError == 1, TRUE, FALSE, missing = FALSE),
         TrackLength = `Length(m)`)

# remove bailingeffor events from eventdata
huntingteam_event <- huntingteam_event %>%
  filter(!(HuntingEventNumber %in% unique(bailingeffortdata$HuntingEventNumber)))

huntingteam_bailing <- bailingeffortdata %>%
  transmute(HuntingEventNumber,
         Hunter = HunterName,
         TrackFileError = TrackFileMissingorCorruptororIncomplete,
         TrackFileError = if_else(TrackFileError == 1, TRUE, FALSE, missing = FALSE),
         TrackLength = `Length(m)`)

huntingteam_faraday <- faradayevent %>%
  transmute(HuntingEventNumber,
         Hunter = LeadHunterName,
         TrackFileError = NA,
         TrackLength = NA)
  
huntingteam <- bind_rows(huntingteam_event, huntingteam_bailing, huntingteam_faraday)

# give unique hunter name if in same event (e.g. Boat1, Boat2, Dog1, Dog2)
huntingteam <- huntingteam %>%
  # get rid of original numbering scheme because inconsistent
  mutate(Hunter = gsub('[[:digit:]]+', '', Hunter)) %>%
  group_by(HuntingEventNumber, Hunter) %>%
  mutate(n = 1:n(),
         n2 = n()) %>%
  ungroup() %>%
  mutate(Hunter = if_else(n2 > 1, p0(Hunter, n), Hunter))

check_key(huntingteam, c("HuntingEventNumber", "Hunter"))
########## huntingteam table issues ##########
# 1. Why are trackLengths different for bailingeffort events in bailingeffort table vs eventdata table
# (They are also not the sum of each individual team member)
# 2. Why are there no tracks for the faraday events?

message("nmight have to get track distance from time based on model")
########## encounter table ###########
# primary key HuntingEventNumber, EncounterID
# joins with event table by HuntingEventNumber
encounter <- killobsdata %>%
  transmute(HuntingEventNumber = gsub(" ", "", HuntingEventNumber),
            EncounterID = RecordNumber,
            DateTimeEncounter = ISOdatetime(Year, Month, Day, EncounterHour, 
                                            EncounterMinute, 0L, tz = tz_data),
            DateTimeEncounter = dtt_adjust_tz(DateTimeEncounter, tz = tz_analysis),
            DeerStatus = DeerStatus,
            DeerLifestage = DeerLifestage,
            DeerSex = DeerSex,
            DNASampleCode = if_else(DNASampleCode == "NA", NA_character_, DNASampleCode),
            DNASample = if_else(tolower(DNASample) != "yes", FALSE, TRUE),
            ShorelineKillSubtype = if_else(SubTypeKillTechnique == "NA", NA_character_, SubTypeKillTechnique),
            Longitude,
            Latitude)

faradaykill <- faradaykill %>%
  transmute(HuntingEventNumber,
            EncounterID = EncounterNumber,
            Longitude, 
            Latitude,
            DateTimeEncounter = ISOdatetime(EncounterYear, EncounterMonth, 
                                            EncounterDay, EncounterHour, 
                                            EncounterMinute, 0L, tz = tz_data),
            DateTimeEncounter = dtt_adjust_tz(DateTimeEncounter, tz = tz_analysis),
            ## all were killed
            DeerStatus = DeerStatus,
            DeerLifestage = DeerLifestage,
            DeerSex = DeerSex,
            DNASampleCode = if_else(DNASampleCode == "NA", NA_character_, DNASampleCode),
            DNASample = if_else(tolower(DNASample) != "yes", FALSE, TRUE),
            Comments)

### deal with coordinates
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

check_key(encounter, c("HuntingEventNumber", "EncounterID"))
anti_join(encounter, event, "HuntingEventNumber")

encounter %<>% ps_coords_to_sfc(c("Longitude", "Latitude"), crs = 4326)

# there are no cases  where there is DNASampleCode but no sex info
missing_sex <- filter(encounter, (DNASample) & is.na(DeerSex))
chk_true(identical(nrow(missing_sex), 0L))

########## encounter table issues ##########
# 1. in faraday data why are there cases of DNASample No but DNASampleCode
wrong_dnasample <- encounter %>%
  filter(!DNASample & !is.na(DNASampleCode))
message("get rid of DNASample TRUE/FALSE")
# 2. There are coords in the ocean that need to be fixed
mapview::mapview(encounter)

# 3. There are two coords with comments: Wpt 006 on Yo Dang and Wpt 007 on Yo Dang...does this mean anything to you? removing for now
message("Robyn can get coors from these waypoints")
# 4. There are 4 missing datetimes
message("look for other kills in event? but just leave for now")
# 5. Why is EncounterID not unique? There are 3 cases of reused EncounterIDs
x <- ps_duplicates(encounter %>% ps_deactivate_sfc(), "EncounterID")
#A214 change to A114b
# add b to the later event for the others
# 6. What happened to HuntingEventNumber RB-110? (exists in encounter table but not event data)
message("dont leave hunter name out of encounter table")
########## age table ##########
#### age data from DNASamples
agedata %<>% 
  filter(Age != "X") %>%
  transmute(ToothID = as.integer(`Tooth ID`), 
            Age = as.integer(Age))

### fix date times
x <- agesourcedata
x$Time[is.na(x$Time)] <- "00"
x$Hour <- x$Time
x$Hour[nchar(x$Time) == 4] <- substr(x$Time, 1, 2)[nchar(x$Time) == 4] 
x$Hour[nchar(x$Time) == 3] <- substr(x$Time, 1, 1)[nchar(x$Time) == 3] 
x$Hour[nchar(x$Time) == 2] <- substr(x$Time, 1, 2)[nchar(x$Time) == 2] 
x$Hour[x$Hour == "00"] <- NA_character_

x$Minute <- x$Time
x$Minute[nchar(x$Time) == 4] <- substr(x$Time, 3, 4)[nchar(x$Time) == 4] 
x$Minute[nchar(x$Time) == 3] <- substr(x$Time, 2, 3)[nchar(x$Time) == 3] 
x$Minute[nchar(x$Time) == 2] <- "00"

agesourcedata <- x

agesourcedata %<>% 
  mutate(DateTimeAge = ISOdatetime(Year, Month, Day, Hour, Minute, 0L, tz = tz_data),
         DateTimeAge = dtt_adjust_tz(DateTimeAge, tz = tz_analysis),
         ToothID = as.integer(ToothID),
         Sex = if_else(tolower(Sex) == "m", "Male", "Female", missing = NA_character_)) %>%
  select(ToothID, SampleID, Island, DateTimeAge, HuntEvent, Sex)

chk_subset(agesourcedata$Sex, c("Male", "Female", NA))
check_join(agedata, agesourcedata, "ToothID")

age <- left_join(agesourcedata, agedata, "ToothID")
age$Island <- paste(age$Island, "Island")

age$Island[age$Island == "Hot Spring Island"] <- "Hotsprings Island"
age$Island[age$Island == "Sgaan Gwaii Island"] <- "Sgang Gwaay Island"

### fix encounter DNASampleCodes so can join to age table
# remove spaces
encounter %<>% ps_deactivate_sfc()
x <- filter(encounter, !is.na(DNASampleCode))
x$DNASampleCode <- gsub(" ", "", x$DNASampleCode)
x$DNASampleCode <- gsub("_", "-", x$DNASampleCode)
x$DNASampleCode[x$DNASampleCode == "D136-RB517"] <- "D136-RB-517"

# add leading 0s that were removed at some point
x$first <- str_split_fixed(x$DNASampleCode, "-", 2)[,1]
x$second <- substring(x$first, 2)
x$first <- substr(x$first, 1, 1)
x$first <- p0(x$first, formatC(as.numeric(x$second), width = 3, flag = "0"))

x$second <- str_split_fixed(x$DNASampleCode, "-", 3)[,2]
unique(x$second)
x$third <- str_split_fixed(x$DNASampleCode, "-", 3)[,3]
x$third <- formatC(as.numeric(x$third), width = 3, flag = "0")
chk_true(all(nchar(x$third) == 3))

x$DNASampleCode <- p0(x$first, "-", x$second, x$third)

encounter$DNASampleCode2 <- encounter$DNASampleCode
encounter$DNASampleCode <- NULL
encounter <- x %>%
  select(EncounterID, HuntingEventNumber, DNASampleCode) %>%
  right_join(encounter, c("HuntingEventNumber", "EncounterID"))

tmp <- encounter %>% 
  select(HuntingEventNumber, EncounterID, DNASampleCode, DNASampleCode2)
View(tmp)


########## age table issues ##########
# 1. Each record in age table should join with an encounter in encounter table based on DNASampleCode
# there are many cases where cannot match
x <- anti_join(age, tmp, c("SampleID" = "DNASampleCode"))

# 2. How did get DeerStage and DeerSex columns in encounter table if impossible to connect to age table?
# 3. Should remove DeerStage, DeerSex, DNASampleCode from encounter table and add EncounterID to age table
message("move sex back to encoutner table because determined by hunter not dna")
message("fix obvious samplecode errors by joining on datetime, send robyn file of remining problems")

########## lookups ##########
hunter <- tibble(Hunter = unique(huntingteam$Hunter))
island <- tibble(Island = unique(event$Island))
huntingtype <- tibble(HuntingType = unique(event$HuntingType))
deerlifestage <- tibble(DeerLifeStage = setdiff(unique(encounter$DeerLifestage), NA))
deersex <- tibble(DeerSex = setdiff(unique(encounter$DeerSex), NA))
deerstatus <- tibble(DeerStatus = c("Killed", "Observed", "Wounded"))

sbf_set_sub("prepare")
sbf_save_data(event)
sbf_save_data(age)
sbf_save_data(encounter)
sbf_save_data(huntingteam)
sbf_save_data(hunter)
sbf_save_data(island)
sbf_save_data(huntingtype)
sbf_save_data(deerstatus)
sbf_save_data(deerlifestage)
sbf_save_data(deersex)

