source("header.R")

sbf_set_sub("read")

sbf_load_datas()


sbf_set_sub("clean", rm = TRUE)

sbf_save_datas()
