source("header.R")

year <- "2020c"

islands <- read_csv("input/islands.csv")
event <- read_csv("input/event20210313.csv")
encounter <- read_csv("input/encounter20210313.csv")
huntingteam <- read_csv("input/huntingteam20210313.csv")
costs <- read_csv("input/costs20210313.csv")

#saves in directory as objects
sbf_set_sub("read", rm = TRUE)

sbf_save_datas()
