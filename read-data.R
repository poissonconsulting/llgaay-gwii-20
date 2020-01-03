source("header.R")

effortdata <- read_csv(file.path(dir, year, "2017/Effort_Calculations/2017_BailerHunting01Nov2017.csv"))

eventdata <- read_xlsx(file.path(dir,year, "2017/2017_MasterHuntingEvents21Nov2017.xlsx"), sheet="MasterHuntingEvents17Oct2017")

killobsdata<- read_xlsx(file.path(dir,year, "2017/2017_MasterKillsandObservations2019Feb15.xlsx"), sheet= "2017_Master_Kills_and_Obs")

agesourcedata <- read_xlsx(file.path(dir,year, "DeerSampleMasterList20190130.xlsx"))
agedata <-read_xlsx(file.path(dir,year, "AgeAnalysis20191121.xlsx"), sheet= "agedata")


#saves in directory as objects
sbf_set_sub("read", rm = TRUE)

sbf_save_datas()
