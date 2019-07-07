#-----------------------------------------#

air<-read.csv("G:/Dataset/data.csv")
View(air)

#air$date <- as.Date(air$date, "%y-%m-%d")
summary(air)
#table(air$type)

#some data cleanup
air$type[air$type=="Sensitive Area"]<- "Sensitive Area"
air$type[air$type %in% c("Industrial","Industrial Area")]<- "Industrial Area"
air$type[air$type %in% c("Residential")]<- "Residential and others"

View(air)

air$date <- as.POSIXct(air$date)
air$year <- year(air$date)

Delhi <- air%>%filter(state == "Delhi")%>% group_by(year, type)%>%summarise(Avg_So2=mean(so2,na.rm=TRUE),
                                                                                 Avg_No2=mean(no2,na.rm=TRUE),
                                                                                 Avg_Rspm=mean(rspm,na.rm=TRUE), 
                                                                                 Avg_Spm= mean(spm,na.rm=TRUE))
Delhi                                                                                 

ggplot(Delhi, aes(x= year, y = Avg_So2))+
  geom_line(size = 1, color = "darkred")+
  geom_point()+
  facet_wrap(~type)+
  ggtitle("Average So2 content year wise")+
  xlab("year")+
  ylab("So2")


ggplot(Delhi, aes(x= year, y = Avg_No2))+
  geom_line(size = 1, color = "darkred")+
  geom_point()+
  facet_wrap(~type)+
  ggtitle("Average No2 content year wise")+
  xlab("year")+
  ylab("No2")


ggplot(Delhi, aes(x= year, y = Avg_Spm))+
  geom_line(size = 1, color = "darkred")+
  geom_point()+
  facet_wrap(~type)+
  ggtitle("Average Spm content year wise")+
  xlab("year")+
  ylab("Spm")

#--------------------------------------------#
