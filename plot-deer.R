source("header.R")

sbf_set_sub("tidy")
sbf_load_datas()

sbf_set_sub("deer")

data <- inner_join(encounter, event, by = "HuntingEventNumber")


gp <- ggplot(data, aes(Age)) + 
  geom_bar(aes(fill = Sex))+  NULL
sbf_open_window()
sbf_print(gp)
sbf_save_plot(x_name='sexage', caption="Count of animals grouped by age and sex.")

class(data$DateTimeEncounter)
y<-filter(data, year(DateTimeEncounter)<2018)
gp<- ggplot (y, aes(DateTimeEncounter))+
  geom_histogram(aes(fill=Status))+NULL
sbf_open_window()
sbf_print(gp)

#RB554 D147 and RB553 D146 are both noted as Jan 30, 2017 by Lenny - No hunting was happening then so they need fixing
#Check with Joe how to proceed - check data? Tidy data? 

gp<- ggplot (y, aes(DateTimeEncounter))+
  geom_histogram(aes(fill=Type))+NULL
sbf_open_window()
sbf_print(gp)


gp<- ggplot(data, aes(Type))+
  geom_bar(aes(fill=Status))
sbf_open_window()
sbf_print(gp)
sbf_save_plot(x_name='agethrutime', caption="Plot of age of deer through time during operations.")

