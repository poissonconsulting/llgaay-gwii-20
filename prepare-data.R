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
         OpportunisticHunting,
         TeamHunting,
         ShorelineWithDog,
         HuntingPhase,
         BaitStationID,
         HuntingTimeCalculated = `Hours(Calc)`,
         CommentEvent = Comments)

### add faraday events
event_faraday <- faradayevent %>%
  transmute(HuntingEventNumber,
            HuntingType,
            Island = paste(IslandName, "Island"), 
            DateTimeOutingStart = ISOdatetime(HuntingStartYear, HuntingStartMonth, 
                                              HuntingStartDay, HuntingStartHour,
                                              HuntingStartMinute, 0L, tz = tz_data), 
            DateTimeOutingEnd = ISOdatetime(HuntingStopYear, HuntingStopMonth, 
                                            HuntingStopDay, HuntingStopHour, 
                                            HuntingStopMinute, 0L, tz = tz_data)) 

event_rb110 <- tibble(HuntingEventNumber = "RB-110", 
                      Island = "Ramsay Island", 
                      HuntingType = "Aerial",
                      DateTimeOutingStart = ymd_hms("2017-05-08 20:10:00", tz = tz_data),
                      DateTimeOutingEnd = ymd_hms("2017-05-08 21:30:00", tz = tz_data))

event <- bind_rows(event, event_faraday, event_rb110)
event$DateTimeOutingStart %<>% dtt_adjust_tz(tz_analysis)
event$DateTimeOutingEnd %<>% dtt_adjust_tz(tz_analysis)

### fix hunting types to match those in metadata
event$HuntingType[event$HuntingType == "Indicator"] <- "Indicator Dog"
event$HuntingType[event$HuntingType == "Bailer"] <- "Bailing Dog"
event$HuntingType[event$HuntingType == "Aerial hot spot hunting"] <- "Aerial Hot Spot Search"
event$HuntingType[event$HuntingType == "Aerial grid search"] <- "Aerial Grid Search"
event$HuntingType[event$HuntingType == "Dog hot spot hunting"] <- "Dog Hot Spot Hunting"

### check that all HuntingEventNumbers accounted for in event table
check_key(event, "HuntingEventNumber")
check_join(bailingeffortdata %>% filter(!is.na(HuntingEventNumber)), event, "HuntingEventNumber")
check_join(faradayevent, event, "HuntingEventNumber")

########## event table issues ##########
# 1. What are the definitions of the all of the time columns? (not in metadata) which to keep?
# 2. Do we need to include NumberOfDogs, NumberOfBoats, etc.?
message("remove NumberOfDogs, etc. cols as not accurate and can be recreated from huntingteam table")
message("(fill in hunter names of faraday huntingteam from kill table where possible)")
message("for now were not going to worry about filling out the team members for early events")
### shoreline until may 20th one shooter, after that might be more team members

# summary <- bailingeffortdata %>%
#   group_by(HuntingEventNumber) %>%
#   summarise(NumberOfDogs = sum(str_detect(tolower(HunterName), "dog")),
#          NumberOfHunters = sum(!str_detect(tolower(HunterName), "dog|boat|helicopter|heli")),
#          NumberOfBoats = sum(str_detect(tolower(HunterName), "boat")),
#          Heli = if_else("helicopter" %in% tolower(HunterName), TRUE, FALSE)) %>%
#   ungroup()
# 
# summary2 <- bailingeffortdata %>%
#   select(HuntingEventNumber, Dogs, Hunters, Boats, Heli) %>%
#   group_by(HuntingEventNumber) %>%
#   slice(1) %>%
#   ungroup()

########## Hunting team table (includes track and situations where multiple team members) ##########
# primary key HuntingEventNumber and Hunter
# TrackFileError has 1 or NA - turn NA into false for eventdata and bailingeffort
# TrackFileError for faradayevent is NA because no information
huntingteam_event <- eventdata %>%
  transmute(HuntingEventNumber,
         Hunter = HunterName,
         HuntLead = TRUE,
         TrackFileError = TrackFileMissingorCorruptororIncomplete,
         TrackFileError = if_else(TrackFileError == 1, TRUE, FALSE, missing = FALSE),
         TrackLength = `Length(m)`)

# remove bailingeffor events from eventdata
huntingteam_event <- huntingteam_event %>%
  filter(!(HuntingEventNumber %in% unique(bailingeffortdata$HuntingEventNumber)))

huntingteam_bailing <- bailingeffortdata %>%
  transmute(HuntingEventNumber,
         Hunter = HunterName,
         HuntLead = HuntLead == "Yes",
         TrackTime = Totaltimeoftrackoverisland,
         TrackFileError = TrackFileMissingorCorruptororIncomplete,
         TrackFileError = if_else(TrackFileError == 1, TRUE, FALSE, missing = FALSE),
         TrackLength = `Length(m)`)

huntingteam_faraday <- map_df(1:nrow(faradayevent), function(a){
  x <- faradayevent[a,]
  id <- x$HuntingEventNumber
  hunters <- rep("Hunter", x$NumberOfHunters) %>% paste0(1:x$NumberOfHunters)
  boats <- rep("Boat", x$NumberOfBoats) %>% paste0(1:x$NumberOfBoats)
  dogs <- rep("Dog", x$NumberOfDogs) %>% paste0(1:x$NumberOfDogs)
  y <- c(x$LeadHunterName, hunters, boats, dogs)
  y <- y[!(y == "1" | y == "0" | y == "Hunter1")]
  tibble(HuntingEventNumber = rep(id, length(y)),
         Hunter = y,
         HuntLead = c(TRUE, rep(FALSE, length(y) - 1)),
         TrackFileError = NA,
         TrackLength = NA)
})

huntingteam_rb110 <- tibble(
  HuntingEventNumber = "RB-110",
  Hunter = "Norm Macdonald",
  HuntLead = TRUE,
  TrackFileError = NA,
  TrackLength = NA
)

huntingteam <- bind_rows(huntingteam_event, huntingteam_bailing,
                         huntingteam_faraday, huntingteam_rb110)

huntingteam$Hunter %<>%
  str_replace("^Lenny\\s+", "Lennard ") %>%
  str_replace("^Pete\\s+", "Peter ") %>%
  str_replace("McLelland$", "McClelland") %>%
  str_replace("^Dog-Patrick Dawson$", "Dog") %>%
  str_replace("^Patrick Dawson [(]from dog[)]$", "Patrick Dawson") %>%
  str_replace("^Gem$", "Dog") %>%
  str_replace("^Meg$", "Dog") %>%
  str_replace("^Bug$", "Dog") %>%
  str_replace("^Sue$", "Dog") %>%
  str_replace("^Helicopter$", "Norm Macdonald") %>%
  str_replace("^Dog1 [(]Lenny[)]$", "Dog") %>%
  gsub('[[:digit:]]+', '', .) %>%
  identity()

# give unique hunter name if in same event (e.g. Boat1, Boat2, Dog1, Dog2)
huntingteam %<>%
  # get rid of original numbering scheme because inconsistent
  group_by(HuntingEventNumber, Hunter) %>%
  mutate(n = 1:n(),
         ntotal = n()) %>%
  ungroup() %>%
  mutate(Hunter = if_else(ntotal > 1, p0(Hunter, n), Hunter)) %>%
  select(-n, -ntotal)

check_key(huntingteam, c("HuntingEventNumber", "Hunter"))

########## huntingteam table issues ##########
# 1. Why are trackLengths different for bailingeffort events in bailingeffort table vs eventdata table
# (They are also not the sum of each individual team member)
message("using track times in bailingeffort data")
# 2. Why are there no tracks for the faraday events?
message("nmight have to get track distance from time based on model for farday")

########## encounter table ###########
# primary key HuntingEventNumber, EncounterID
# joins with event table by HuntingEventNumber
encounter <- killobsdata %>%
  transmute(HuntingEventNumber = gsub(" ", "", HuntingEventNumber),
            EncounterID = RecordNumber,
            Hunter = HunterName,
            DateTimeEncounter = ISOdatetime(Year, Month, Day, EncounterHour, 
                                            EncounterMinute, 0L, tz = tz_data),
            DateTimeEncounter = dtt_adjust_tz(DateTimeEncounter, tz = tz_analysis),
            DeerStatus = DeerStatus,
            DeerLifestage = DeerLifestage,
            DeerSex = DeerSex,
            DNASampleCode = if_else(DNASampleCode == "NA", NA_character_, DNASampleCode),
            ShorelineKillSubtype = if_else(SubTypeKillTechnique == "NA", NA_character_, SubTypeKillTechnique),
            Longitude,
            Latitude)

faradaykill <- faradaykill %>%
  transmute(HuntingEventNumber,
            EncounterID = EncounterNumber,
            Hunter = HunterName,
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
missing_sex <- filter(encounter, (!is.na(DNASampleCode) & is.na(DeerSex)))
chk_true(identical(nrow(missing_sex), 0L))

### fix duplicate encounterID
x <- ps_duplicates(encounter %>% ps_deactivate_sfc(), "EncounterID")
encounter$EncounterID[encounter$HuntingEventNumber == "RB-253" & encounter$EncounterID == "A214"] <- "A214b"
encounter$EncounterID[encounter$HuntingEventNumber == "RB-449" & encounter$EncounterID == "B408"] <- "B408b"
encounter$EncounterID[encounter$HuntingEventNumber == "RB-449" & encounter$EncounterID == "B409"] <- "B409b"

### fix hunter names

encounter$Hunter %<>%
  str_replace("^Lenny\\s+", "Lennard ") %>%
  str_replace("^Pete\\s+", "Peter ") %>%
  str_replace("McLelland", "McClelland") %>%
  identity()

check_key(encounter, "EncounterID")
########## encounter table issues ##########
# 1. in faraday data why are there cases of DNASample No but DNASampleCode
# wrong_dnasample <- encounter %>%
#   filter(!DNASample & !is.na(DNASampleCode))
message("remove DNASample TRUE/FALSE as redundant")
# 2. There are coords in the ocean that need to be fixed
# mapview::mapview(encounter)
message("need to fix coords in ocean")
# 3. There are two coords with comments: Wpt 006 on Yo Dang and Wpt 007 on Yo Dang...does this mean anything to you? removing for now
message("get coords from wpt 006 and 007")
# 4. There are 4 missing datetimes
message("look for other kills in event to fill in missing date times...leaving for now")
# 5. Why is EncounterID not unique? There are 3 cases of reused EncounterIDs
message("renaming duplicate EncounterIDs") 
# 6. What happened to HuntingEventNumber RB-110? (exists in encounter table but not event data)
message("added event RB-110")
########## age table ##########
#### age data from DNASamples

agedata %<>% 
  filter(Age != "X") %>%
  transmute(ToothID = as.integer(`Tooth ID`), 
            Age = as.integer(Age))

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
encounter %<>% rename(CommentEncounter = Comments)

tmp <- encounter %>% 
  select(HuntingEventNumber, EncounterID, DNASampleCode, DNASampleCode2, DateTimeEncounter)
# View(tmp)
encounter$DNASampleCode2 <- NULL

########## age table issues ##########
# 1. Each record in age table should join with an encounter in encounter table based on DNASampleCode
# there are many cases where cannot match
x <- anti_join(age, tmp, c("SampleID" = "DNASampleCode"))
y <- left_join(x, encounter, c("DateTimeAge" = "DateTimeEncounter"))

write_csv(x, "nonmatchingsampleid.csv")

# 2. How did get DeerStage and DeerSex columns in encounter table if impossible to connect to age table?
# 3. Should remove DeerStage, DeerSex, DNASampleCode from encounter table and add EncounterID to age table
message("move sex back to encounter table because determined by hunter not dna")
message("fix obvious samplecode errors by joining on datetime, send robyn file of remining problems")

# ########## fix aerial
# hot_spot_events <- event$HuntingEventNumber[str_detect(tolower(event$HuntingType), "aerial hot spot hunting")]
# grid_events <- event$HuntingEventNumber[str_detect(tolower(event$HuntingType), "aerial grid search")]
# 
# event$HuntingType[str_detect(tolower(event$HuntingType), "aerial")] <- "Aerial"
# encounter$KillSubtype <- encounter$ShorelineKillSubtype
# encounter$ShorelineKillSubtype <- NULL
# 
# encounter$KillSubtype[encounter$HuntingEventNumber %in% hot_spot_events] <- 4
# encounter$KillSubtype[encounter$HuntingEventNumber %in% grid_events] <- 5
# 
# killsubtype <- tibble(KillSubtype = 0:5,
#                       Type = c(rep("Shoreline", 4), rep("Aerial", 2)),
#                       Description = c("observed",
#                                       "shot from boat",
#                                       "shot from shore",
#                                       "active pursuit with indicator dog",
#                                       "hot spot hunting",
#                                       "grid search"))

########## lookups ##########
hunter <- tibble(Hunter = c(unique(huntingteam$Hunter), "Jonas Prevost", "James Nickerson"))
island <- tibble(Island = unique(event$Island))
huntingtype <- tibble(HuntingType = unique(event$HuntingType))
deerlifestage <- tibble(DeerLifeStage = setdiff(unique(encounter$DeerLifestage), NA))
deersex <- tibble(DeerSex = setdiff(unique(encounter$DeerSex), NA))
deerstatus <- tibble(DeerStatus = c("Killed", "Observed", "Wounded"))
shorelinekillsubtype <- tibble(ShorelineKillSubtype = 0:3, 
                               Description = c("observed", "shot from boat", 
                                               "shot from shore",
                                               "active pursuit with indicator dog"))


huntingteam %<>%
  mutate(
    Hunter = if_else(HuntingEventNumber == "RB18-017" & Hunter == "Hunter1", "Tauren Collinson", Hunter),
    Hunter = if_else(HuntingEventNumber == "RB18-017" & Hunter == "Hunter2", "Gerry Morigeau", Hunter),
    Hunter = if_else(HuntingEventNumber == "RB18-019" & Hunter == "Hunter1", "Jay Jones", Hunter),
    Hunter = if_else(HuntingEventNumber == "RB18-019" & Hunter == "Hunter2", "Jonas Prevost", Hunter),
    Hunter = if_else(HuntingEventNumber == "RB18-019" & Hunter == "Hunter3", "Judson Brown", Hunter),
    Hunter = if_else(HuntingEventNumber == "RB18-019" & Hunter == "Hunter4", "Gerry Morigeau", Hunter),
    Hunter = if_else(HuntingEventNumber == "RB18-019" & Hunter == "Hunter5", "Tauren Collinson", Hunter))

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
sbf_save_data(shorelinekillsubtype)

