source("header.R")

sbf_set_sub("clean")

sbf_load_datas()

encounter %<>% 
  left_join(age, by = c(DNASampleCode = "SampleID")) %>%
  select(-DNASampleCode)

rm(age)

sbf_set_sub("tidy", rm = TRUE)

sbf_save_datas()
