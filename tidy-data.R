source("header.R")

sbf_set_sub("clean")

sbf_load_datas()

#bruteforcehistogram  
killobsdata$DeerStatus<-as.factor(killobsdata$DeerStatus)
subkill<-killobsdata[killobsdata$DeerStatus == "Killed", ]  
subobs<-killobsdata[killobsdata$DeerStatus == "Observed",]
   
quartz()
g <- ggplot(subkill, aes(Type))
g + geom_bar()
g + geom_bar(aes(fill = DeerSex))
ggsave("output/DeerSex.png")

quartz()
g <- ggplot(subkill, aes(Type))
g + geom_bar()
g + geom_bar(aes(fill = DeerLifestage))
ggsave("output/DeerLifeStage.png")

quartz()
g<-ggplot(subkill, aes(Island))
g + geom_bar()
g + geom_bar(aes(fill = DeerLifestage))
ggsave("output/DeerLifeStagebyIsland.png")

sbf_set_sub("tidy")

sbf_save_datas()
