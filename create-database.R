source("header.R")

conn <- sbf_open_db(project, exists = FALSE)

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
                "CREATE TABLE ShorelineKillSubtype (
                ShorelineKillSubtype INTEGER NOT NULL,
                ShorelineKillSubtypeDescription TEXT NOT NULL)")

rws_write(shorelinekillsubtype, conn = conn)

DBI::dbGetQuery(conn,
                "CREATE TABLE Event (
                HuntingEventNumber TEXT PRIMARY KEY NOT NULL,
                HuntingType TEXT NOT NULL,
                Island TEXT NOT NULL,
                DateTimeOutingStart TEXT NOT NULL,
                DateTimeOutingEnd TEXT NOT NULL,
                Hunter TEXT NOT NULL,
                NumberOfHunters INTEGER,
                NumberOfDogs INTEGER,
                NumberOfBoats INTEGER,
                Heli BOOLEAN,
                OpportunisticHunting BOOLEAN,
                TeamHunting BOOLEAN,
                ShorelineWithDog BOOLEAN,
                HuntingPhase INTEGER,
                HuntingTime INTEGER,
                TrackOverIslandTime INTEGER,
                HuntingTimeCalculated INTEGER,
                TrackFileError BOOLEAN,
                CommentOuting TEXT,
                FOREIGN KEY (Island) REFERENCES Island (Island),
                FOREIGN KEY (HuntingType) REFERENCES HuntingType (HuntingType),
                FOREIGN KEY (Hunter) REFERENCES Hunter (Hunter))")

rws_write(event, conn = conn)

DBI::dbGetQuery(conn,
                "CREATE TABLE Track (
                OutingID TEXT NOT NULL,
                Hunter TEXT NOT NULL,
                TrackLength REAL NOT NULL,
                PRIMARY KEY (OutingID, Hunter),
                FOREIGN KEY (OutingID) REFERENCES Outing (OutingID))")

rws_write(track, conn = conn)

DBI::dbGetQuery(conn,
                "CREATE TABLE Encounter (
                OutingID TEXT NOT NULL,
                EncounterID TEXT NOT NULL,
                DateTimeEncounter TEXT,
                Killed BOOLEAN NOT NULL,
                DeerLifeStage TEXT,
                DeerSex TEXT,
                TrackLength REAL NOT NULL,
                PRIMARY KEY (OutingID, EncounterID),
                FOREIGN KEY (OutingID) REFERENCES Outing (OutingID))")

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
