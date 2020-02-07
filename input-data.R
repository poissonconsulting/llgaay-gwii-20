source("header.R")

sbf_load_datas_from_db("llgaay-gwii", rename = tolower)

sbf_set_sub("input", rm = TRUE)
sbf_save_datas()
