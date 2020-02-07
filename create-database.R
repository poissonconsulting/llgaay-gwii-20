source("header.R")

conn <- sbf_open_db("llgaay-gwii", exists = FALSE)

sbf_set_sub("prepare")
sbf_load_datas()

DBI::dbGetQuery(conn,
                "CREATE TABLE Island (
                Island TEXT PRIMARY KEY NOT NULL)")

rws_write(island, conn = conn)

DBI::dbGetQuery(conn,
                "CREATE TABLE Hunter (
                Hunter TEXT PRIMARY KEY NOT NULL)")

rws_write(hunter, conn = conn)

DBI::dbGetQuery(conn,
                "CREATE TABLE HuntingType (
                HuntingType TEXT PRIMARY KEY NOT NULL)")

rws_write(huntingtype, conn = conn)

DBI::dbGetQuery(conn,
                "CREATE TABLE DeerLifeStage (
                DeerLifeStage TEXT PRIMARY KEY NOT NULL)")

rws_write(deerlifestage, conn = conn)

DBI::dbGetQuery(conn,
                "CREATE TABLE DeerSex (
                DeerSex TEXT PRIMARY KEY NOT NULL)")

rws_write(deersex, conn = conn)

DBI::dbGetQuery(conn,
                "CREATE TABLE DeerStatus (
                DeerStatus TEXT PRIMARY KEY NOT NULL)")

rws_write(deerstatus, conn = conn)

DBI::dbGetQuery(conn,
                "CREATE TABLE ShorelineKillSubtype (
                ShorelineKillSubtype INTEGER PRIMARY KEY NOT NULL,
                Description TEXT NOT NULL)")

rws_write(shorelinekillsubtype, conn = conn)

DBI::dbGetQuery(conn,
                "CREATE TABLE Event (
                HuntingEventNumber TEXT PRIMARY KEY NOT NULL,
                HuntingType TEXT NOT NULL,
                Island TEXT NOT NULL,
                DateTimeOutingStart TEXT NOT NULL,
                DateTimeOutingEnd TEXT NOT NULL,
                OpportunisticHunting BOOLEAN,
                TeamHunting BOOLEAN,
                ShorelineWithDog BOOLEAN,
                HuntingPhase INTEGER,
                HuntingTimeCalculated INTEGER,
                BaitStationID TEXT,
                CommentEvent TEXT,
                FOREIGN KEY (Island) REFERENCES Island (Island),
                FOREIGN KEY (HuntingType) REFERENCES HuntingType (HuntingType))")

rws_write(event, conn = conn)

DBI::dbGetQuery(conn,
                "CREATE TABLE HuntingTeam (
                HuntingEventNumber TEXT NOT NULL,
                Hunter TEXT NOT NULL,
                TrackLength REAL,
                TrackFileError BOOLEAN,
                PRIMARY KEY (HuntingEventNumber, Hunter),
                FOREIGN KEY (HuntingEventNumber) REFERENCES Event (HuntingEventNumber))")

rws_write(huntingteam, conn = conn)

DBI::dbGetQuery(conn,
                "CREATE TABLE Encounter (
                EncounterID TEXT NOT NULL,
                HuntingEventNumber TEXT NOT NULL,
                DateTimeEncounter TEXT,
                Hunter TEXT NOT NULL,
                DeerStatus BOOLEAN NOT NULL,
                DeerLifeStage TEXT,
                DeerSex TEXT,
                ShorelineKillSubtype INTEGER,
                DNASampleCode TEXT,
                CommentEncounter TEXT,
                geometry TEXT,
                PRIMARY KEY (EncounterID),
                FOREIGN KEY (HuntingEventNumber) REFERENCES event (HuntingEventNumber),
                FOREIGN KEY (DeerStatus) REFERENCES DeerStatus (DeerStatus),
                FOREIGN KEY (DeerLifeStage) REFERENCES DeerLifeStage (DeerLifeStage),
                FOREIGN KEY (DeerSex) REFERENCES DeerSex (DeerSex),
                FOREIGN KEY (Hunter) REFERENCES Hunter (Hunter),
                FOREIGN KEY (ShorelineKillSubtype) REFERENCES ShorelineKillSubtype (ShorelineKillSubtype))")

rws_write(encounter, conn = conn)

DBI::dbGetQuery(conn,
                "CREATE TABLE Age (
                ToothID INTEGER PRIMARY KEY NOT NULL,
                SampleID TEXT,
                Island TEXT NOT NULL,
                DateTimeAge TEXT,
                HuntEvent TEXT NOT NULL,
                Sex TEXT,
                Age INTEGER,
                FOREIGN KEY (Island) REFERENCES Island (Island),
                FOREIGN KEY (Sex) REFERENCES DeerSex (DeerSex))")

rws_write(age, conn = conn)

sbf_close_db(conn)
