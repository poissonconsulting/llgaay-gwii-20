source("header.R")

count <- read_csv(file.path(dir, year, "count.csv"))

species <- fishbc::freshwaterfish

sbf_set_sub("read", rm = TRUE)

sbf_save_datas()
