source("header.R")
library (scales)

sbf_set_sub("clean")
sbf_load_datas()

data <- inner_join(event, encounter, by = "HuntingEventNumber")

sbf_set_sub("deer")

#scatterplot to determine which effort metric to use (are they predictive on the 1:1 line and what is the bias)
# gp<-ggplot (data=event, aes(Hours,TrackTime, colour=Type))+
#   geom_point()
# sbf_open_window()
# sbf_print(gp)

#sex ratio by age for deer that were sexed which is most (%?)
gp <- ggplot(data, aes(Age)) + 
  geom_bar(aes(fill = Sex))+  NULL
sbf_open_window()
sbf_print(gp)
sbf_save_plot(x_name='sexage', caption="Count of animals grouped by age and sex.")


y<-filter(data, year(DateTimeEncounter)<2018)
y<-filter(y, month(DateTimeEncounter)<9)
z<-filter(data, year(DateTimeEncounter)>=2018)


gp<- ggplot (y, aes(DateTimeEncounter))+
  geom_histogram(aes(fill=Type), binwidth=1*3600*24*7)+NULL+
  xlab("Time") + ylab("Deer Killed") +labs(fill="Hunting Method")
gp <- gp + scale_x_datetime(breaks=date_breaks("1 month"))#, labels = date_format("%d/%m"))
gp<- gp + limits=c(as.POSIXct('2017/04/15'), as.POSIXct('2017/10/31'))
sbf_open_window()
sbf_print(gp)
sbf_save_plot(x_name='huntingtypethrutime2017', caption="Plot of number of deer killed by hunting method through time for 2017 summer operations.")


gp<- ggplot (y, aes(DateTimeEncounter))+
  geom_histogram(aes(fill=Type), binwidth=1*3600*24*7)+NULL+
  xlab("Time") + ylab("Deer Killed") +labs(fill="Hunting Method")
gp <- gp + scale_x_datetime(breaks=date_breaks("1 month"), labels = date_format("%b"))
gp <- gp + facet_wrap(~Island)
sbf_open_window()
sbf_print(gp)
sbf_save_plot(x_name='huntypetimebyisl2017', caption="Plot of number of deer killed by hunting method through time by island for 2017 summer operations.")


gp<- ggplot (z, aes(DateTimeEncounter))+
  geom_histogram(aes(fill=Type), binwidth=1*3600*24*3)+NULL+
  xlab("Time") + ylab("Deer Killed") +labs(fill="Hunting Method")
gp <- gp + scale_x_datetime(breaks=date_breaks("1 week"))#, labels = date_format("%d/%m"))
sbf_open_window()
sbf_print(gp)
sbf_save_plot(x_name='huntingtypethrutime2018', caption="Plot of number of deer killed by hunting method through time for 2018 operations.")




gp<- ggplot (y, aes(DateTimeEncounter))+
  geom_histogram(aes(fill=Status))+NULL
sbf_open_window()
sbf_print(gp)



gp<- ggplot(data, aes(Type))+
  geom_bar(aes(fill=Status))
sbf_open_window()
sbf_print(gp)
sbf_save_plot(x_name='agethrutime', caption="Plot of age of deer through time during operations.")


#I've had the rubber duck experience and found that the with date the documentation meant specifically an vector of the class Date. The behaviour of binwidthwith the class POSIXct is described in the followup sentence: the bin width of a time variable is the number of seconds.

#In short, the solution is multiplying binwidth by 3600*24 to get days instead of seconds.

#ggplot(df,aes(day)) + 
 # geom_histogram(binwidth = 1*3600*24,colour = "black",fill = "grey")
