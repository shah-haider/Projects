library(gganimate)
library(maps)
library(ggplot2)
library(readxl)
library(dplyr)
library(maps)
library(ggmap)
library(tidyr)
#install.packages("rgdal")
library(rgdal)
library(ggplot2)
library(gganimate)
#install.packages("animation")
library(animation)
remove.packages("rlang")
install.packages("rlang")
#install.packages('transformr')
library(transformr)
library(gapminder)

ll <- read_excel("ll.xlsx")
MapData <- split(RegressionClusterData,f = RegressionClusterData$Year)
MapData <- as.data.frame(MapData[4])
MapData <- merge(x = MapData, y = ll, by.x = "X2019.Country", by.y ="Country", all.x = TRUE)

#GDPaddition
GDPtoadd <- split(GDP_growth, f = GDP_growth$Year)
GDPtoadd <- as.data.frame(GDPtoadd[4])
GDPtoadd <- na.omit(GDPtoadd)
GDPtoadd <- GDPtoadd[,-2]
MapData <- merge(x = MapData, y = GDPtoadd, by = "X2019.Country", all.x = TRUE)

names(MapData)
str(MapData)

#VISUALIZATION FOR CLUSTERS WHEN K=4

latlimit <- c(-40,68)
lonlimit <- c(-95,170)
MapData$X2019.Cluster <- as.factor(MapData$X2019.Cluster)
p <- ggplot()
p <- p + geom_polygon( data=world_map, aes(x=long, y=lat, group = group),colour="white")+
  geom_point( data=MapData, aes(x=Lon, y=Lat, color = X2019.Cluster, size=4) )+ scale_size(name="Clusters")+
  geom_text( data=MapData, hjust=0.03, vjust=-0.03, aes(x=Lon, y=Lat, label=X2019.Country), colour="red", size=3)+
  coord_cartesian(xlim = lonlimit, ylim= latlimit)+
  labs(title="Clusters",x= "Longitude", y ="Latitude" )
p
range(MapData$X2019.GDP_growth.y)
subset <- filter(K4, Cluster == 1 | Cluster == 2 | Cluster == 3 | Cluster == 4 )
write.csv(subset, file ="clusterdata.csv")

#Suicide vs GDP with line of best fit 
str(FinalData)
A= ggplot( data=FinalData, aes(x=GDP_growth, y=Suicide_All)) +
  geom_point(aes(color=factor(Year))) +
  geom_smooth(se=FALSE, data = FinalData, method='lm', formula= y~x,aes(color=factor(Year)) ) +
  ylab('Suicide Rate') +
  xlab('GDP Growth') + ggtitle('Suicide vs GDP')+
  theme(legend.position=c(0.94,0.9), legend.title=element_blank()) 
print(A)

#Suicide vs Alcohol Use with line of best fit 
B= ggplot( data=FinalData, aes(x=AlcoholUse, y=Suicide_All)) +
  geom_point(aes(color=factor(Year))) +
  geom_smooth(se=FALSE, data = FinalData, method='lm', formula= y~x,aes(color=factor(Year)) ) +
  ylab('Suicide Rate') +
  xlab('Alcohol Use') + ggtitle('Suicide vs Alcholo Use')+
  theme(legend.position=c(0.94,0.9), legend.title=element_blank()) 
print(B)

#Suicide vs Depression with line of best fit 
C= ggplot( data=FinalData, aes(x=Depression, y=Suicide_All)) +
  geom_point(aes(color=factor(Year))) +
  geom_smooth(se=FALSE, data = FinalData, method='lm', formula= y~x,aes(color=factor(Year)) ) +
  ylab('Suicide Rate') +
  xlab('Depression') + ggtitle('Suicide vs Depression ')+
  theme(legend.position=c(0.94,0.9), legend.title=element_blank()) 
print(C)

#Suicide vs HDI with line of best fit 
D= ggplot( data=FinalData, aes(x=UP_all, y=Suicide_All)) +
  geom_point(aes(color=factor(Year))) +
  geom_smooth(se=FALSE, data = FinalData, method='lm', formula= y~x,aes(color=factor(Year)) ) +
  ylab('Suicide Rate') +
  xlab('Unemployment') + ggtitle('Suicide vs Unemployment ')+
  theme(legend.position=c(0.94,0.9), legend.title=element_blank()) 
print(D)

#Suicide vs Anxiety with line of best fit 
E= ggplot( data=FinalData, aes(x=Anxiety, y=Suicide_All)) +
  geom_point(aes(color=factor(Year))) +
  geom_smooth(se=FALSE, data = FinalData, method='lm', formula= y~x,aes(color=factor(Year)) ) +
  ylab('Suicide Rate') +
  xlab('Anxiety') + ggtitle('Suicide vs Anxiety ')+
  theme(legend.position=c(0.94,0.9), legend.title=element_blank()) 
print(E)

#### MAPPINGs ###

map.world <- map_data("world")
map.world_joined <- right_join(map.world, FinalData, by = c('region' = 'Country'))
#practivce
world_map <- map_data("world")


#graphs cluster 1
PanelDatasetC4_1_join <- right_join(map.world, PanelDatasetC4_1, by = c('region' = 'Country'))
#AlcoholUse
ggplot(PanelDatasetC4_1_join, aes(map_id= region, fill=AlcoholUse)) +
  geom_map(map = map.world, colour="black") +
  scale_fill_binned(type = "viridis")+
  expand_limits(x=PanelDatasetC4_1_join$long, y=PanelDatasetC4_1_join$lat)

#graphs cluster 2
PanelDatasetC4_2_join <- right_join(map.world, PanelDatasetC4_2, by = c('region' = 'Country'))
#AlcoholUse
ggplot(PanelDatasetC4_2_join, aes(map_id= region, fill=AlcoholUse)) +
  geom_map(map = map.world, colour="black") +
  scale_fill_binned(type = "viridis")+
  expand_limits(x=PanelDatasetC4_2_join$long, y=PanelDatasetC4_2_join$lat)

#Depression
ggplot(PanelDatasetC4_2_join, aes(map_id= region, fill=Depression)) +
  geom_map(map = map.world, colour="black") +
  scale_fill_binned(type = "viridis")+
  expand_limits(x=PanelDatasetC4_2_join$long, y=PanelDatasetC4_2_join$lat)

#graphs cluster 3
PanelDatasetC4_3_join <- right_join(map.world, PanelDatasetC4_3, by = c('region' = 'Country'))
#AlcoholUse
ggplot(PanelDatasetC4_3_join, aes(map_id= region, fill=AlcoholUse)) +
  geom_map(map = map.world, colour="black") +
  scale_fill_binned(type = "viridis")+
  expand_limits(x=PanelDatasetC4_3_join$long, y=PanelDatasetC4_3_join$lat)
#GDP_growth
ggplot(PanelDatasetC4_3_join, aes(map_id= region, fill=GDP_growth)) +
  geom_map(map = map.world, colour="black") +
  scale_fill_binned(type = "viridis")+
  expand_limits(x=PanelDatasetC4_3_join$long, y=PanelDatasetC4_3_join$lat)



#graphs cluster 4
PanelDatasetC4_4_join <- right_join(map.world, PanelDatasetC4_4, by = c('region' = 'Country'))
#Depression
ggplot(PanelDatasetC4_4_join, aes(map_id= region, fill=Depression)) +
  geom_map(map = map.world, colour="black") +
  scale_fill_binned(type = "viridis")+
  expand_limits(x=PanelDatasetC4_4_join$long, y=PanelDatasetC4_4_join$lat)
#Anxiety
ggplot(PanelDatasetC4_4_join, aes(map_id= region, fill=Anxiety)) +
  geom_map(map = map.world, colour="black") +
  scale_fill_binned(type = "viridis")+
  expand_limits(x=PanelDatasetC4_4_join$long, y=PanelDatasetC4_4_join$lat)

#######################################################3
#Scatters for variables VS Suicide_All in .gifs

##########################################################



Combined_data <- RegressionClusterData[Year]
Combined_data <- RegressionClusterData[RegressionClusterData$Year == 2019, ]
Combined_data$Cluster <- as.factor(Combined_data$Cluster)
na.omit(Combined_data)

#UP_All
ggplot(Combined_data, aes(UP_all, Suicide_All, color = Cluster, size = 3, stroke = 1, alpha = 0.7)) +
  geom_point() +
  # geom_line()+
  geom_smooth(method = lm, size = 1, alpha = 0.7, se=FALSE)+
  #  facet_wrap(~Cluster)+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  transition_states(
    Cluster,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink()

#GDP_growth
ggplot(Combined_data, aes(GDP_growth, Suicide_All, color = Cluster, size = 3, stroke = 1, alpha = 0.7)) +
  geom_point() +
  # geom_line()+
  geom_smooth(method = lm, size = 1, alpha = 0.7, se=FALSE)+
  #  facet_wrap(~Cluster)+
  #  scale_x_continuous(expand = c(0, 0))+
  #  scale_y_continuous(expand = c(0, 0))+
  labs(title= "UP_All_scatter")+
  transition_states(
    Cluster,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink()

#GDP
ggplot(Combined_data, aes(GDP, Suicide_All, color = Cluster, size = 3, stroke = 1, alpha = 0.7)) +
  geom_point() +
  # geom_line()+
  geom_smooth(method = lm, size = 1, alpha = 0.7, se=FALSE)+
  #  facet_wrap(~Cluster)+
  # scale_x_continuous(expand = c(0, 0))+
  #  scale_y_continuous(expand = c(0, 0))+
  labs(title= "GDP_scatter")+
  transition_states(
    Cluster,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink()

#Depression
ggplot(Combined_data, aes(Depression, Suicide_All, color = Cluster, size = 3, stroke = 1, alpha = 0.7)) +
  geom_point() +
  # geom_line()+
  geom_smooth(method = lm, size = 1, alpha = 0.7, se=FALSE)+
  #  facet_wrap(~Cluster)+
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  labs(title= "Depression_scatter")+
  transition_states(
    Cluster,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink()

#AlcoholUse
ggplot(Combined_data, aes(AlcoholUse, Suicide_All, color = Cluster, size = 3, stroke = 1, alpha = 0.7)) +
  geom_point() +
  # geom_line()+
  geom_smooth(method = lm, size = 1, alpha = 0.7, se=FALSE)+
  #  facet_wrap(~Cluster)+
  # scale_x_continuous(expand = c(0, 0))+
  #  scale_y_continuous(expand = c(0, 0))+
  labs(title= "AlcoholUse_scatter")+
  transition_states(
    Cluster,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink()

#happiness
ggplot(Combined_data, aes(happiness, Suicide_All, color = Cluster, size = 3, stroke = 1, alpha = 0.7)) +
  geom_point() +
  # geom_line()+
  geom_smooth(method = lm, size = 1, alpha = 0.7, se=FALSE)+
  #  facet_wrap(~Cluster)+
  # scale_x_continuous(expand = c(0, 0))+
  #  scale_y_continuous(expand = c(0, 0))+
  labs(title= "happiness_scatter")+
  transition_states(
    Cluster,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink()

#Schizophrenia
ggplot(Combined_data, aes(Schizophrenia, Suicide_All, color = Cluster, size = 3, stroke = 1, alpha = 0.7)) +
  geom_point() +
  # geom_line()+
  geom_smooth(method = lm, size = 1, alpha = 0.7, se=FALSE)+
  #  facet_wrap(~Cluster)+
  # scale_x_continuous(expand = c(0, 0))+
  #  scale_y_continuous(expand = c(0, 0))+
  labs(title= "Schizophrenia_scatter")+
  transition_states(
    Cluster,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink()

#Bipolar
ggplot(Combined_data, aes(Bipolar, Suicide_All, color = Cluster, size = 3, stroke = 1, alpha = 0.7)) +
  geom_point() +
  # geom_line()+
  geom_smooth(method = lm, size = 1, alpha = 0.7, se=FALSE)+
  #  facet_wrap(~Cluster)+
  # scale_x_continuous(expand = c(0, 0))+
  #  scale_y_continuous(expand = c(0, 0))+
  labs(title= "Bipolar_scatter")+
  transition_states(
    Cluster,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink()

#Eating
ggplot(Combined_data, aes(Eating, Suicide_All, color = Cluster, size = 3, stroke = 1, alpha = 0.7)) +
  geom_point() +
  # geom_line()+
  geom_smooth(method = lm, size = 1, alpha = 0.7, se=FALSE)+
  #  facet_wrap(~Cluster)+
  # scale_x_continuous(expand = c(0, 0))+
  # scale_y_continuous(expand = c(0, 0))+
  labs(title= "Eating_scatter")+
  transition_states(
    Cluster,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink()

#Anxiety
ggplot(Combined_data, aes(Anxiety, Suicide_All, color = Cluster, size = 3, stroke = 1, alpha = 0.7)) +
  geom_point() +
  # geom_line()+
  geom_smooth(method = lm, size = 1, alpha = 0.7, se=FALSE)+
  #  facet_wrap(~Cluster)+
  # scale_x_continuous(expand = c(0, 0))+
  # scale_y_continuous(expand = c(0, 0))+
  labs(title= "Anxiety_scatter")+
  transition_states(
    Cluster,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink()


#DrugUse
ggplot(Combined_data, aes(DrugUse, Suicide_All, color = Cluster, size = 3, stroke = 1, alpha = 0.7)) +
  geom_point() +
  # geom_line()+
  geom_smooth(method = lm, size = 1, alpha = 0.7, se=FALSE)+
  #  facet_wrap(~Cluster)+
  #scale_x_continuous(expand = c(0, 0))+
  #  scale_y_continuous(expand = c(0, 0))+
  labs(title= "DrugUse_scatter")+
  transition_states(
    Cluster,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink()

#HDIRank
ggplot(Combined_data, aes(HDIRank, Suicide_All, color = Cluster, size = 3, stroke = 1, alpha = 0.7)) +
  geom_point() +
  # geom_line()+
  geom_smooth(method = lm, size = 1, alpha = 0.7, se=FALSE)+
  #  facet_wrap(~Cluster)+
  # scale_x_continuous(expand = c(0, 0))+
  #  scale_y_continuous(expand = c(0, 0))+
  labs(title= "HDIRank_scatter")+
  transition_states(
    Cluster,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink()

