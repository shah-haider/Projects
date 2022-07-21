library(readxl)
library(reshape2)
library(dplyr)
library(ExPanDaR)
library(NbClust)
library(ggplot2)
library(factoextra)

library(tidyverse)
#library(rvest)
library(magrittr)
#install.packages("maps")
library(ggmap)
library(stringr)

getwd()
setwd("H:/7th semester/BA/grp prj")

FinalData <- read.csv("FinalData.csv")




# Correlation Plot
library(ggcorrplot)
cor_data <- FinalData[,c(-1,-2)]
cor_data <- scale(cor_data)
corr <- round(cor(cor_data), 1)
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)



#scatters of high and low co-relations
theme_set(theme_bw())  # pre-set the bw theme.

g <- ggplot(FinalData, aes(Eating, Bipolar, size = Suicide_All)) +
  geom_point() +
  geom_smooth(aes(group = Year), 
              method = "lm", 
              show.legend = FALSE) +
  facet_wrap(~Country, scales = "free") +
  scale_x_log10() +
  transition_manual(Year) # convert to log scale

animate(g, interval=0.2)




library(maps)


map.world <- map_data("world")




write.csv(map.world, "world_mp.csv")



FinalData$Country [FinalData$Country == 'United States'] <- 'USA'          # Replace b by XXX
FinalData$Country [FinalData$Country == 'United Kingdom'] <- 'UK'          # Replace b by XXX
FinalData$Country [FinalData$Country == 'Egypt, Arab Rep.'] <- 'Egypt'          # Replace b by XXX
FinalData$Country [FinalData$Country == 'Russian Federation'] <- 'Russia'          # Replace b by XXX
FinalData$Country [FinalData$Country == 'Korea, Rep.'] <- 'South Korea'          # Replace b by XXX
FinalData$Country [FinalData$Country == 'Iran, Islamic Rep.'] <- 'Iran'          # Replace b by XXX
FinalData$Country [FinalData$Country == 'Congo'] <- 'Democratic Republic of the Congo'          # Replace b by XXX



FinalData$Country <- recode(FinalData$Country
                                   ,'United States' = 'USA'
                                   ,'United Kingdom' = 'UK'
                                   ,'Egypt, Arab Rep.' = 'Egypt'
                                   ,'Russian Federation' = 'Russia'
                                   ,'Korea, Rep.' = 'South Korea'
                                   ,'Iran, Islamic Rep.' = 'Iran'
                                   ,'Congo' = 'Democratic Republic of the Congo'
                            )
                            


map.world_joined <- right_join(map.world, FinalData, by = c('region' = 'Country'))

europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","UK")
europeanUnion <- data.frame(europeanUnion)
EU_join <- inner_join(europeanUnion, map.world_joined, by = c('europeanUnion' = 'region'))


library(ggplot2)
library(ggmap)
#install.packages("gapminder")
library(ggalt)
library(gganimate)
library(gapminder)


ggplot(map.world_joined, aes(map_id= region, fill=Suicide_All)) +
  geom_map(map = world_map, colour="black") +
  scale_fill_continuous(type = "viridis")+
  geom_encircle(data = EU_join, size = 2, color = "blue")+
  expand_limits(x=map.world_joined$long, y=map.world_joined$lat)

#High corelation b/w alcoholuse and suicide
ggplot(map.world_joined, aes(map_id= region, fill=AlcoholUse)) +
  geom_map(map = world_map, colour="black") +
  scale_fill_binned(type = "viridis")+
  expand_limits(x=map.world_joined$long, y=map.world_joined$lat)

ggplot(map.world_joined, aes(map_id= region, fill=AlcoholUse)) +
  geom_map(map = world_map, colour="black") +
  scale_fill_binned(type = "viridis")+
  expand_limits(x=map.world_joined$long, y=map.world_joined$lat)

#p <= ggplot(mtcars, aes(mpg, wt))+ geom_point()
#p + expand_limits(x = 0, y = 0)



Data2019 <- subset(FinalData, Year == "2019")
DM_Data <- Data2019[,c(-1,-2,-19)]
DM_Data <- scale(DM_Data)
DM_Data <- as.data.frame(DM_Data)
DM <- dist(as.matrix(DM_Data))
AvgHC <-hclust(DM, method="average")
plot(AvgHC, hang = -1, cex=0.8 ,labels = Data2019$Country)

HDI <- read_excel("RawDataset.xlsx" , sheet = 18)
HDI <- distinct(HDI)


#8clusters
clustCut8<-cutree(AvgHC,k=8)
clustCut8
table(clustCut8)
o=order(clustCut8)
cluster8 <- data.frame(Data2019$Country[o], clustCut8[o])
colnames(cluster8) <- c("Country", "Cluster")
rect.hclust(AvgHC, k=8, border="blue")
cluster8 <- merge(x = cluster8, y = HDI , by = "Country", all = FALSE)
fviz_cluster(list(data=DM_Data, cluster= clustCut8)) + geom_text(label=cluster8$Country, size = 3, vjust=0)


map.world_joined <- right_join(map.world, FinalData, by = c('region' = 'Country'))


p <- ggplot()
p <- p + geom_polygon( data=map.world, aes(x=long, y=lat, group = group),colour="white")
p <- p + geom_point( data=FinalData, aes(x=lon, y=lat, color = cluster8), size = 5)+ scale_size(name="Clusters")
p <- p + geom_text( data=comp_data_clust, hjust=0.03, vjust=-0.03, aes(x=Lon, y=Lat, label=Country), colour="red", size=3)+
  coord_cartesian(xlim = lonlimit, ylim= latlimit)+ labs(title="Clusters",x= "Longitude", y ="Latitude" )
p




df.country_points <- data.frame(country = c("Singapore","Luxembourg"),stringsAsFactors = F)
glimpse(df.country_points)
#--------
# GEOCODE
#--------
geocode.country_points <- geocode(df.country_points$country)
df.country_points <- cbind(df.country_points,geocode.country_points)
# INSPECT
print(df.country_points)