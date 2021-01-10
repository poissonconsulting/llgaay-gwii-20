source("header.R")

sbf_set_sub("tidy")
sbf_load_datas()

sbf_set_sub("deer")

data <- inner_join(encounter, event, by = "HuntingEventNumber")


