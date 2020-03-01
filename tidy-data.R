source("header.R")

sbf_set_sub("clean")

sbf_load_datas()

groupedbyvars<- group_by(encounter,HuntingEventNumber) %>%
  summarise(Total = n()) %>%
  ungroup()

encenv<-left_join(event,groupedbyvars, by="HuntingEventNumber")
encenv%<>% replace_na(list(Total=0L))

sbf_set_sub("tidy", rm = TRUE)

sbf_save_datas()

#x <- 10

#x %<>% log() %>% sqrt()

#x


