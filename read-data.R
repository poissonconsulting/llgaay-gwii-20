source("header.R")

year <- "2020c"

event <- read_csv("input/event.csv")
encounter <- read_csv("input/encounter.csv")
huntingteam <- read_csv("input/huntingteam.csv")
costs <- read_csv("input/costs.csv")

#saves in directory as objects
sbf_set_sub("read", rm = TRUE)

sbf_save_datas()
