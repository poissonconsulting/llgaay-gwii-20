source("header.R")

year <- "2020b"

bailingeffortdata <- read_csv(file.path(dir, year, "2017/Effort_Calculations/2017_BailerHunting01Nov2017.csv"))

eventdata <- read_xlsx(file.path(dir,year, "2017/2017_MasterHuntingEvents20201121.xlsx"), sheet="MasterHuntingEvents17Oct2017")

killobsdata<- read_xlsx(file.path(dir,year, "2017/2017_MasterKillsandObservations20201121.xlsx"), sheet= "2017_Master_Kills_and_Obs")

agesourcedata <- read_xlsx(file.path(dir,year, "DeerSampleMasterList20201121.xlsx"))
agedata <-read_xlsx(file.path(dir,year, "AgeAnalysis20191121.xlsx"), sheet= "agedata")

faradayevent <- read_xlsx(file.path(dir, year, "DatasheetsFaraday2018 FINAL.xlsx"), sheet = "HuntingEventsFaraday2018")
faradaykill <- read_xlsx(file.path(dir, year, "DatasheetsFaraday2018 FINAL.xlsx"), sheet = "HuntingEncountersFaraday2018")

#saves in directory as objects
sbf_set_sub("read", rm = TRUE)

sbf_save_datas()
