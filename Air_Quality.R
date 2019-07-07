library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

#we are setting the working directory for data
#for use of data--------------------------------
list.files("G:/Dataset")

air<-read.csv("G:/Dataset/data.csv")
View(air)

air$date <- as.Date(air$date, "%y-%m-%d")
summary(air)
table(air$type)

#some data cleanup
air$type[air$type=="Sensitive Area"]<- "Sensitive Area"
air$type[air$type %in% c("Industrial","Industrial Area")]<- "Industrial Area"
air$type[air$type %in% c("Residential")]<- "Residential and others"

by_state_wise <-air%>%group_by(state)%>%summarise(Avg_So2=mean(so2,na.rm = TRUE),
                                                  Avg_No2=mean(no2,na.rm = TRUE),
                                                  Avg_Rspm=mean(rspm,na.rm = TRUE),
                                                  Avg_spm=mean(spm,na.rm = TRUE))

by_state_wise

##plot between so2 and state------------------------
ggplot(by_state_wise, aes(x= state, y=Avg_So2, fill=Avg_So2))+
  geom_bar(stat="identity")+
  theme(axis.text.x =element_text(angle=90))+
  ggtitle("Average So2 content state wise")+
  xlab(label = "state")+
  ylab(label = "So2")

##plot between no2 and state--------------------
ggplot(by_state_wise, aes(x= state, y=Avg_No2, fill=Avg_No2))+
  geom_bar(stat="identity")+
  theme(axis.text.x =element_text(angle=90))+
  ggtitle("Average No2 content state wise")+
  xlab(label = "state")+
  ylab(label = "No2")

##plot between rspm and state-----------------------
ggplot(by_state_wise, aes(x= state, y=Avg_Rspm, fill=Avg_Rspm))+
  geom_bar(stat="identity")+
  theme(axis.text.x =element_text(angle=90))+
  ggtitle("Average Rspm content state wise")+
  xlab(label = "state")+
  ylab(label = "Rspm")

##plot  between spm and state---------------------
ggplot(by_state_wise, aes(x= state, y=Avg_spm, fill=Avg_spm))+
  geom_bar(stat="identity")+
  theme(axis.text.x =element_text(angle=90))+
  ggtitle("Average spm content state wise")+
  xlab(label = "state")+
  ylab(label = "spm")

#--------------------------------#