source("header.R")

year <- "2020c"

event <- read_csv(file.path(dir, year, "event.csv"))
encounter <- read_csv(file.path(dir, year, "encounter.csv"))
huntingteam <- read_csv(file.path(dir, year, "huntingteam.csv"))

#saves in directory as objects
sbf_set_sub("read", rm = TRUE)

sbf_save_datas()
