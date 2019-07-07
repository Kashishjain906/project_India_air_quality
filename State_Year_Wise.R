#--------------------------------------------#
View(air)

library(xts)
library(reshape2)

#some data cleanup
air$type[air$type=="Sensitive Area"]<- "Sensitive Area"
air$type[air$type %in% c("Industrial","Industrial Area")]<- "Industrial Area"
air$type[air$type %in% c("Residential")]<- "Residential and others"


State_Year_Wise <-air %>%group_by(state,year) %>%summarise(So2=mean(so2,na.rm=TRUE),
                                                               No2=mean(no2,na.rm=TRUE),
                                                               Rspm=mean(rspm,na.rm=TRUE),
                                                               spm=mean(spm,na.rm=TRUE))

State_Year_Wise.long <-melt(State_Year_Wise,id=c("year","state"),measure=c("So2","No2","Rspm","spm"))
State_Year_Wise.long

ggplot(State_Year_Wise.long,aes(x=year,y=value,color=variable)) +
  geom_bar(stat="identity",position="dodge") +
  facet_wrap(~state)


ggplot(State_Year_Wise,aes(x=year,y=So2)) +
  geom_bar(stat="identity",position="dodge") +
  facet_wrap(~state)

State_Year_Wise.long %>%
  filter(variable=="So2") %>%
  ggplot(aes(x=year,y=state,fill=value)) +
  geom_tile(color="white") +
  scale_fill_gradient(low="white",high="steelblue") +
  theme(legend.position = "right") +
  labs(title="Heat Map for Average SO2 Content")

State_Year_Wise.long %>%
  filter(variable=="No2") %>%
  ggplot(aes(x=year,y=state,fill=value)) +
  geom_tile(color="white") +
  scale_fill_gradient(low="white",high="steelblue") +
  theme(legend.position = "right") +
  labs(title="Heat Map for Average Nitrous Oxide Content")


State_Year_Wise.long %>%
  filter(variable=="Rspm") %>%
  ggplot(aes(x=year,y=state,fill=value)) +
  geom_tile(color="white") +
  scale_fill_gradient(low="white",high="steelblue") +
  theme(legend.position = "right") +
  labs(title="Heat Map for Average RSPM Content")

#------------------------------------------#