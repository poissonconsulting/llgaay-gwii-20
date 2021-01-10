source("header.R")

sbf_set_sub("tidy")
sbf_load_datas()

sbf_set_sub("deer")

data <- inner_join(encounter, event, by = "HuntingEventNumber")

gp<- ggplot(data, aes(Type))+
  geom_bar(aes(fill=Status))
sbf_open_window()
sbf_print(gp)
sbf_save_plot(x_name='agethrutime', caption="Plot of age of deer through time during operations.")

