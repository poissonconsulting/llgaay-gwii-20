source("header.R")

sbf_set_sub("clean")
sbf_load_datas()



sbf_set_sub("tidy", rm = TRUE)
sbf_save_datas()