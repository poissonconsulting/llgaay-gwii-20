source("header.R")
library (scales)

sbf_set_sub("clean")
sbf_load_datas()

data <- inner_join(event, encounter, by = "HuntingEventNumber")

sbf_set_sub("deer")

y<-filter(data, year(DateTimeEncounter)<2018)
#y<-filter(y, month(DateTimeEncounter)<9)
z<-filter(data, year(DateTimeEncounter)>=2018)


gp<- ggplot (y, aes(DateTimeEncounter))+
  geom_histogram(aes(fill=Method), binwidth=1*3600*24*7)+NULL+
  xlab("Time") + ylab("Deer Killed") +labs(fill="Hunting Method")
gp <- gp + scale_x_datetime(breaks=date_breaks("1 month"), labels = date_format("%b"))
sbf_open_window()
sbf_print(gp)
sbf_save_plot(x_name='huntingtypethrutime2017', caption="Plot of number of deer killed by hunting method through time for 2017 operations.")


gp<- ggplot (y, aes(DateTimeEncounter))+
  geom_histogram(aes(fill=Method), binwidth=1*3600*24*7)+NULL+
  xlab("Time") + ylab("Deer Killed") +labs(fill="Hunting Method")
gp <- gp + scale_x_datetime(breaks=date_breaks("1 month"), labels = date_format("%b"))
gp <- gp + facet_wrap(~IslandHaida)
sbf_open_window()
sbf_print(gp)
sbf_save_plot(x_name='huntypetimebyisl2017', caption="Plot of number of deer killed by hunting method through time by island for 2017 summer operations.")


gp<- ggplot (z, aes(DateTimeEncounter))+
  geom_histogram(aes(fill=Method), binwidth=1*3600*24*3)+NULL+
  xlab("Time") + ylab("Deer Killed") +labs(fill="Hunting Method")
gp <- gp + scale_x_datetime(breaks=date_breaks("1 week"), labels = date_format("%b-%d"))
gp <- gp + facet_wrap(~IslandHaida)
sbf_open_window()
sbf_print(gp)
sbf_save_plot(x_name='huntingtypethrutime2018', caption="Plot of number of deer killed by hunting method through time for 2018 operations.")

# gp<- ggplot(data, aes(Method))+
#   geom_bar(aes(fill=Status))
# sbf_open_window()
# sbf_print(gp)
# sbf_save_plot(x_name='agethrutime', caption="Plot of age of deer through time during operations.")

#scatterplot to determine which effort metric to use (are they predictive on the 1:1 line and what is the bias)
# gp<-ggplot (data=event, aes(Hours,TrackTime, colour=Method))+
#   geom_point()
# sbf_open_window()
# sbf_print(gp)

#sex ratio by age for deer that were sexed which is most (%?)
# gp <- ggplot(data, aes(Age)) + 
#   geom_bar(aes(fill = Sex))+  NULL
# sbf_open_window()
# sbf_print(gp)
# sbf_save_plot(x_name='sexage', caption="Count of animals grouped by age and sex.")

