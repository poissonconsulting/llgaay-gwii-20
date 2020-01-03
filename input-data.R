source("header.R")

conn <- sbf_open_db(project)

species <- rws_read_table("Species", conn = conn)
count <- rws_read_table("Count", conn = conn)

sbf_set_sub("input", rm = TRUE)
sbf_save_datas()
