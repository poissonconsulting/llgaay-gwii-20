source("header.R")

sbf_set_sub("tidy")
sbf_load_datas()


sbf_set_sub("plot")

gp <- ggplot(encounter, aes(Age)) + 
geom_bar(aes(fill = DeerSex))+  NULL
sbf_open_window()
sbf_print(gp)

sbf_save_plot(x_name = "AgeStackBar", caption = "A plot frequency of different ages of animals by sex.")
#can scale the plot to be proportional - look at stack overflow

gp<- ggplot(event, aes(PrimaryHuntingType))+
  geom_bar(aes(fill=DeerStatus))

gp <- ggplot(encounter, aes(x=DateTimeEncounter,y=Age))+
  geom_point(alpha=1/3)
  NULL

sbf_open_window()
sbf_print(gp)
sbf_save_plot(x_name='agethrutime', caption="Plot of age of deer through time during operations.")

gp <-ggplot(event, aes(x=DateTimeOutingStart, y=HuntingTimeCalculated))+
  geom_point()
  NULL

sbf_open_window()
sbf_print(gp)
sbf_save_plot(x_name='numbersoverallthrutime', caption="Plot of total time by event killed or observed through time.")

killsonly<-filter(encounter, Culled =="TRUE")
gp<- ggplot(killsonly, aes(x=Hunter))+
  geom_bar(stat='count')
gp
