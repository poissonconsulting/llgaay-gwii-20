source("header.R")

sbf_set_sub("tidy")

age<-sbf_load_data("age")
encounter<-sbf_load_data("encounter")
event<-sbf_load_data("event")
encenv<-sbf_load_data("encenv")
encenv$Year<- year(encenv$DateTimeOutingStart)
  
sbf_set_sub("plot")

gp <- ggplot(age, aes(Age)) + 
  geom_bar(aes(fill = Sex))+
  NULL

sbf_open_window()
print(gp)
sbf_save_plot(x_name = "AgeStackBar", caption = "A plot frequency of different ages of animals by sex.")
#can scale the plot to be proportional - look at stack overflow

gp <- ggplot(age, aes(x=DateTimeAge,y=Age))+
  geom_point(alpha=1/3)+
  facet_wrap(~Island, scale="free_x")+
  NULL

sbf_open_window()
print(gp)
sbf_save_plot(x_name='agethrutime', caption="Plot of age of deer through time during operations.")

gp <-ggplot(encenv, aes(x=DateTimeOutingStart, y=Total))+
  geom_point()+
  facet_wrap(~Year,scale="free_x")+
  NULL

sbf_open_window()
print(gp)
sbf_save_plot(x_name='numbersoverallthrutime', caption="Plot of totals killed or observed through time.")


gp<- ggplot(event, aes(x=))
