source("header.R")

sbf_set_sub("tidy")
sbf_load_datas()

sbf_set_sub("deer")

#scatterplot to determine which effort metric to use (are they predictive on the 1:1 line and what is the bias)
gp<-ggplot (data=effdata, aes(DiffTime,TrackTime, colour=Type))+
  geom_point()
sbf_open_window()
sbf_print(gp)

#sex ratio by age for deer that were sexed which is most (%?)
gp <- ggplot(data, aes(Age)) + 
  geom_bar(aes(fill = Sex))+  NULL
sbf_open_window()
sbf_print(gp)
sbf_save_plot(x_name='sexage', caption="Count of animals grouped by age and sex.")


y<-filter(data, year(DateTimeEncounter)<2018)
gp<- ggplot (y, aes(DateTimeEncounter))+
  geom_histogram(aes(fill=Status))+NULL
sbf_open_window()
sbf_print(gp)

gp<- ggplot (y, aes(DateTimeEncounter))+
  geom_histogram(aes(fill=Type))+NULL
sbf_open_window()
sbf_print(gp)


gp<- ggplot(data, aes(Type))+
  geom_bar(aes(fill=Status))
sbf_open_window()
sbf_print(gp)
sbf_save_plot(x_name='agethrutime', caption="Plot of age of deer through time during operations.")

