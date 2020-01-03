source("header.R")

sbf_set_sub("read")

sbf_load_datas()

# select, filter and rename data
count %<>% dplyr::rename(Count = count)

species %<>% select(Species = Code, CommonName)

sbf_set_sub("clean", rm = TRUE)

sbf_save_datas()
