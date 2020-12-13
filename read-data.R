source("header.R")

year <- "2020c"

event <- read_csv(file.path(dir, year, "encounter.csv"))
encounter <- read_csv(file.path(dir, year, "encounter.csv"))

#saves in directory as objects
sbf_set_sub("read", rm = TRUE)

sbf_save_datas()
