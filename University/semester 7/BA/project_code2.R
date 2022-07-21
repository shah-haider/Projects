library(readxl)
library(reshape2)
library(dplyr)
library(ExPanDaR)
library(NbClust)
library(ggplot2)
library(factoextra)
#
library(AER)
library(panelr)
library(lmtest)
library(plm)
library(ggcorrplot)
#
#haider added packages
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


setwd("H:/7th semester/BA/grp prj")

Suicide_male <- read_excel("RawDataset.xlsx" , sheet = 1)
LongData <- melt(Suicide_male, id.vars=c("Country"))
colnames(LongData)[2] <- ("Year")
colnames(LongData)[3] <- ("Suicide_Male")
Suicide_male <- LongData

Suicide_female <- read_excel("RawDataset.xlsx" , sheet = 2)
LongData <- melt(Suicide_female, id.vars=c("Country"))
colnames(LongData)[2] <- ("Year")
colnames(LongData)[3] <- ("Suicide_Female")
Suicide_female <- LongData

Suicide_all <- read_excel("RawDataset.xlsx" , sheet = 3)
LongData <- melt(Suicide_all, id.vars=c("Country"))
colnames(LongData)[2] <- ("Year")
colnames(LongData)[3] <- ("Suicide_All")
Suicide_all <- LongData

UP_male <- read_excel("RawDataset.xlsx" , sheet = 4)
LongData <- melt(UP_male, id.vars=c("Country"))
colnames(LongData)[2] <- ("Year")
colnames(LongData)[3] <- ("UP_male")
UP_male <- LongData

UP_female <- read_excel("RawDataset.xlsx" , sheet = 5)
LongData <- melt(UP_female, id.vars=c("Country"))
colnames(LongData)[2] <- ("Year")
colnames(LongData)[3] <- ("UP_female")
UP_female <- LongData

UP_all <- read_excel("RawDataset.xlsx" , sheet = 6)
LongData <- melt(UP_all, id.vars=c("Country"))
colnames(LongData)[2] <- ("Year")
colnames(LongData)[3] <- ("UP_all")
UP_all <- LongData

GDP_growth <- read_excel("RawDataset.xlsx" , sheet = 7)
LongData <- melt(GDP_growth, id.vars=c("Country"))
colnames(LongData)[2] <- ("Year")
colnames(LongData)[3] <- ("GDP_growth")
GDP_growth <- LongData

GDP <- read_excel("RawDataset.xlsx" , sheet = 8)
LongData <- melt(GDP, id.vars=c("Country"))
colnames(LongData)[2] <- ("Year")
colnames(LongData)[3] <- ("GDP")
GDP <- LongData

happiness <- read_excel("RawDataset.xlsx" , sheet = 9)
LongData <- melt(happiness, id.vars=c("Country"))
colnames(LongData)[2] <- ("Year")
colnames(LongData)[3] <- ("happiness")
happiness <- LongData

Depression <- read_excel("RawDataset.xlsx" , sheet=10 )
LongData <- melt(Depression, id.vars=c("Country"))
colnames(LongData)[2] <- ("Year")
colnames(LongData)[3] <- ("Depression")
Depression <- LongData

AlcoholUse <- read_excel("RawDataset.xlsx" , sheet=11 )
LongData <- melt(AlcoholUse, id.vars=c("Country"))
colnames(LongData)[2] <- ("Year")
colnames(LongData)[3] <- ("AlcoholUse")
AlcoholUse <- LongData

Schizophrenia <- read_excel("RawDataset.xlsx" , sheet=12 )
LongData <- melt(Schizophrenia, id.vars=c("Country"))
colnames(LongData)[2] <- ("Year")
colnames(LongData)[3] <- ("Schizophrenia")
Schizophrenia <- LongData

Bipolar <- read_excel("RawDataset.xlsx" , sheet=13 )
LongData <- melt(Bipolar, id.vars=c("Country"))
colnames(LongData)[2] <- ("Year")
colnames(LongData)[3] <- ("Bipolar")
Bipolar <- LongData

Eating <- read_excel("RawDataset.xlsx" , sheet=14 )
LongData <- melt(Eating, id.vars=c("Country"))
colnames(LongData)[2] <- ("Year")
colnames(LongData)[3] <- ("Eating")
Eating <- LongData

Anxiety <- read_excel("RawDataset.xlsx" , sheet=15 )
LongData <- melt(Anxiety, id.vars=c("Country"))
colnames(LongData)[2] <- ("Year")
colnames(LongData)[3] <- ("Anxiety")
Anxiety <- LongData

DrugUse <- read_excel("RawDataset.xlsx" , sheet=16 )
LongData <- melt(DrugUse, id.vars=c("Country"))
colnames(LongData)[2] <- ("Year")
colnames(LongData)[3] <- ("DrugUse")
DrugUse <- LongData

Source1_List <- list(Suicide_all, UP_male, UP_female, UP_all, GDP_growth, GDP)
Source2_List <- list(happiness, Schizophrenia, Bipolar, Eating, Anxiety, DrugUse )

Source1_Merged <- merge(x = Suicide_male, y = Suicide_female, by = c("Country","Year"), all = TRUE)
for (i in Source1_List) {
  Source1_Merged <- merge(x = Source1_Merged, y = i , by = c("Country","Year"), all = TRUE)
}

Source2_Merged <- merge(x = Depression, y = AlcoholUse, by = c("Country","Year"), all = TRUE)
for (j in Source2_List) {
  Source2_Merged <- merge(x = Source2_Merged, y = j , by = c("Country","Year"), all = TRUE)
}

FinalMerged <- merge(x = Source1_Merged, y = Source2_Merged, by = c("Country","Year"), all = TRUE)
FinalData <- na.omit(FinalMerged)
HDI <- read_excel("RawDataset.xlsx" , sheet = 18)
FinalData$HDIRank <- HDI$`HDI Rank`
row.names(FinalData) <- NULL
FinalData <- FinalData[c(-126,-128,-130,-132),]

FinalData$Country [FinalData$Country == 'United States'] <- 'USA'          # Replace b by XXX
FinalData$Country [FinalData$Country == 'United Kingdom'] <- 'UK'          # Replace b by XXX
FinalData$Country [FinalData$Country == 'Egypt, Arab Rep.'] <- 'Egypt'          # Replace b by XXX
FinalData$Country [FinalData$Country == 'Russian Federation'] <- 'Russia'          # Replace b by XXX
FinalData$Country [FinalData$Country == 'Korea, Rep.'] <- 'South Korea'          # Replace b by XXX
FinalData$Country [FinalData$Country == 'Iran, Islamic Rep.'] <- 'Iran'          # Replace b by XXX
FinalData$Country [FinalData$Country == 'Congo'] <- 'Democratic Republic of the Congo'          # Replace b by XXX


write.csv(FinalData, "FinalData.csv")

#ExPanD(df = FinalData, cs_id = "Country", ts_id = "Year" )

HDI <- distinct(HDI)

#HClust
Data2019 <- subset(FinalData, Year == "2019")
DM_Data <- Data2019[,c(-1,-2,-10,-19)]
DM_Data <- DM_Data[,c(-1,-2,-4,-5)]
DM_Data <- scale(DM_Data)
DM_Data <- as.data.frame(DM_Data)
DM <- dist(as.matrix(DM_Data))
AvgHC <-hclust(DM, method="average")
plot(AvgHC, hang = -1, cex=0.8 ,labels = Data2019$Country)

#10clusters
clustCut10<-cutree(AvgHC,k=10)
clustCut10
table(clustCut10)
o=order(clustCut10)
cluster10 <- data.frame(Data2019$Country[o], clustCut10[o])
colnames(cluster10) <- c("Country", "Cluster")
rect.hclust(AvgHC, k=10, border="blue")
cluster10 <- merge(x = cluster10, y = HDI , by = "Country", all = FALSE)
fviz_cluster(list(data=DM_Data, cluster= clustCut10)) + geom_text(label=cluster10$Country, size = 3, vjust=0)

#9clusters
clustCut9<-cutree(AvgHC,k=9)
clustCut9
table(clustCut9)
o=order(clustCut9)
cluster9 <- data.frame(Data2019$Country[o], clustCut9[o])
colnames(cluster9) <- c("Country", "Cluster")
rect.hclust(AvgHC, k=9, border="blue")
cluster9 <- merge(x = cluster9, y = HDI , by = "Country", all = FALSE)
fviz_cluster(list(data=DM_Data, cluster= clustCut9)) + geom_text(label=cluster9$Country, size = 3, vjust=0)

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

#7clusters
clustCut7<-cutree(AvgHC,k=7)
clustCut7
table(clustCut7)
o=order(clustCut7)
cluster7 <- data.frame(Data2019$Country[o], clustCut7[o])
colnames(cluster7) <- c("Country", "Cluster")
rect.hclust(AvgHC, k=7, border="blue")
cluster7 <- merge(x = cluster7, y = HDI , by = "Country", all = FALSE)
fviz_cluster(list(data=DM_Data, cluster= clustCut7)) + geom_text(label=cluster7$Country, size = 3, vjust=0)

#6clusters
clustCut6<-cutree(AvgHC,k=6)
clustCut6
table(clustCut6)
o=order(clustCut6)
cluster6 <- data.frame(Data2019$Country[o], clustCut6[o])
colnames(cluster6) <- c("Country", "Cluster")
rect.hclust(AvgHC, k=6, border="blue")
cluster6 <- merge(x = cluster6, y = HDI , by = "Country", all = FALSE)
fviz_cluster(list(data=DM_Data, cluster= clustCut6)) + geom_text(label=cluster6$Country, size = 3, vjust=0)

#5clusters
clustCut5<-cutree(AvgHC,k=5)
clustCut5
table(clustCut5)
o=order(clustCut5)
cluster5 <- data.frame(Data2019$Country[o], clustCut5[o])
colnames(cluster5) <- c("Country", "Cluster")
rect.hclust(AvgHC, k=5, border="blue")
cluster5 <- merge(x = cluster5, y = HDI , by = "Country", all = FALSE)
fviz_cluster(list(data=DM_Data, cluster= clustCut5)) + geom_text(label=cluster5$Country, size = 3, vjust=0)

#4clusters
clustCut4<-cutree(AvgHC,k=4)
clustCut4
table(clustCut4)
o=order(clustCut4)
cluster4 <- data.frame(Data2019$Country[o], clustCut4[o])
colnames(cluster4) <- c("Country", "Cluster")
rect.hclust(AvgHC, k=4, border="blue")
cluster4 <- merge(x = cluster4, y = HDI , by = "Country", all = FALSE)
fviz_cluster(list(data=DM_Data, cluster= clustCut4)) + geom_text(label=cluster4$Country, size = 3, vjust=0)

#3clusters
clustCut3<-cutree(AvgHC,k=3)
clustCut3
table(clustCut3)
o=order(clustCut3)
cluster3 <- data.frame(Data2019$Country[o], clustCut3[o])
colnames(cluster3) <- c("Country", "Cluster")
rect.hclust(AvgHC, k=3, border="blue")
cluster3 <- merge(x = cluster3, y = HDI , by = "Country", all = FALSE)
fviz_cluster(list(data=DM_Data, cluster= clustCut3)) + geom_text(label=cluster3$Country, size = 3, vjust=0)

#2clusters
clustCut2<-cutree(AvgHC,k=2)
clustCut2
table(clustCut2)
o=order(clustCut2)
cluster2 <- data.frame(Data2019$Country[o], clustCut2[o])
colnames(cluster2) <- c("Country", "Cluster")
rect.hclust(AvgHC, k=2, border="blue")
cluster2 <- merge(x = cluster2, y = HDI , by = "Country", all = FALSE)
fviz_cluster(list(data=DM_Data, cluster= clustCut2)) + geom_text(label=cluster2$Country, size = 3, vjust=0)

#Optimal
Nb<- NbClust(DM_Data, distance = "euclidean", min.nc = 2, max.nc=10, method = "average", index ="all")
fviz_nbclust(Nb) 

#KMeans
set.seed(51)

#10clusters
kout10 <- kmeans(DM_Data,centers=10,nstart=10)
table(kout10$cluster)
table(Data2019$Country, kout10$cluster)
kout10$cluster
o=order(kout10$cluster)
o
K10 <- data.frame(Data2019$Country[o],kout10$cluster[o])
colnames(K9) <- c("Country", "Cluster")
K10 <- merge(x = K10, y = HDI , by = "Country", all = FALSE)
fviz_cluster(kout10,DM_Data, geom="point") + 
  geom_text(label=Data2019$Country, size = 2.5, vjust=0.2)

#9clusters
kout9 <- kmeans(DM_Data,centers=9,nstart=10)
table(kout9$cluster)
table(Data2019$Country, kout9$cluster)
kout9$cluster
o=order(kout9$cluster)
o
K9 <- data.frame(Data2019$Country[o],kout9$cluster[o])
colnames(K9) <- c("Country", "Cluster")
K9 <- merge(x = K9, y = HDI , by = "Country", all = FALSE)
fviz_cluster(kout9,DM_Data, geom="point") + 
  geom_text(label=Data2019$Country, size = 2.5, vjust=0.2)

#8clusters
kout8 <- kmeans(DM_Data,centers=8,nstart=10)
table(kout8$cluster)
table(Data2019$Country, kout8$cluster)
kout8$cluster
o=order(kout8$cluster)
o
K8 <- data.frame(Data2019$Country[o],kout8$cluster[o])
colnames(K8) <- c("Country", "Cluster")
K8 <- merge(x = K8, y = HDI , by = "Country", all = FALSE)
fviz_cluster(kout8,DM_Data, geom="point") + 
  geom_text(label=Data2019$Country, size = 2.5, vjust=0.2)

#7clusters
kout7 <- kmeans(DM_Data,centers=7,nstart=10)
table(kout7$cluster)
table(Data2019$Country, kout7$cluster)
kout7$cluster
o=order(kout7$cluster)
o
kout7
K7 <- data.frame(Data2019$Country[o],kout7$cluster[o])
colnames(K7) <- c("Country", "Cluster")
K7 <- merge(x = K7, y = HDI , by = "Country", all = FALSE)
fviz_cluster(kout7,DM_Data, geom="point") + 
  geom_text(label=Data2019$Country, size = 2.5, vjust=0.2)

#6clusters
kout6 <- kmeans(DM_Data,centers=6,nstart=10)
table(kout6$cluster)
table(Data2019$Country, kout6$cluster)
kout6$cluster
o=order(kout6$cluster)
o
K6 <- data.frame(Data2019$Country[o],kout6$cluster[o])
colnames(K6) <- c("Country", "Cluster")
K6 <- merge(x = K6, y = HDI , by = "Country", all = FALSE)
fviz_cluster(kout6,DM_Data, geom="point") + 
  geom_text(label=Data2019$Country, size = 2.5, vjust=0.2)

#5 clusters
kout5 <- kmeans(DM_Data,centers=5,nstart=10)
table(kout5$cluster)
table(Data2019$Country, kout5$cluster)
kout5$cluster
o=order(kout5$cluster)
o
K5 <- data.frame(Data2019$Country[o],kout5$cluster[o])
colnames(K5) <- c("Country", "Cluster")
K5 <- merge(x = K5, y = HDI , by = "Country", all = FALSE)

fviz_cluster(kout5,DM_Data, geom="point") + 
  geom_text(label=Data2019$Country, size = 2.5, vjust=0.2)

#4 clusters
kout4 <- kmeans(DM_Data,centers=4,nstart=10)
table(kout4$cluster)
table(Data2019$Country, kout4$cluster)
kout4$cluster
o=order(kout4$cluster)
o
kout4
K4 <- data.frame(Data2019$Country[o],kout4$cluster[o])
colnames(K4) <- c("Country", "Cluster")
K4 <- merge(x = K4, y = HDI , by = "Country", all = FALSE)

fviz_cluster(kout4,DM_Data, geom="point") + 
  geom_text(label=Data2019$Country, size = 2.5, vjust=0.2)

#3 clusters
kout3 <- kmeans(DM_Data,centers=3,nstart=10)
table(kout3$cluster)
table(Data2019$Country, kout3$cluster)
kout3$cluster
o=order(kout3$cluster)
o
K3 <- data.frame(Data2019$Country[o],kout3$cluster[o])
colnames(K3) <- c("Country", "Cluster")
K3 <- merge(x = K4, y = HDI , by = "Country", all = FALSE)

fviz_cluster(kout3,DM_Data, geom="point") + 
  geom_text(label=Data2019$Country, size = 2.5, vjust=0.2)

#2 clusters
kout2 <- kmeans(DM_Data,centers=2,nstart=10)
table(kout2$cluster)
table(Data2019$Country, kout2$cluster)
kout2$cluster
o=order(kout2$cluster)
o
K2 <- data.frame(Data2019$Country[o],kout2$cluster[o])
colnames(K2) <- c("Country", "Cluster")
K2 <- merge(x = K2, y = HDI , by = "Country", all = FALSE)

fviz_cluster(kout2,DM_Data, geom="point") + 
  geom_text(label=Data2019$Country, size = 2.5, vjust=0.2)

##########################REG

CorrData <- FinalData[,c(-1,-2,-19)]
corr <- cor(CorrData)
ggcorrplot(corr, hc.order = FALSE, lab_size = 3, type = "lower",
           lab = TRUE, title="Correlation Matrix for our variables",) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust=0.5, size=10)) +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.4, hjust=0.5, size=10)) 

PanelDataset <- pdata.frame(FinalData[,-19], index = c("Year","Country"))
pdim(PanelDataset)
summary(PanelDataset)

#Model
##################################################################

#Fixed Effects
Regression = plm(Suicide_All ~ UP_all + GDP_growth +
                   Depression + AlcoholUse  + happiness + Schizophrenia +
                   Bipolar + Eating + Anxiety + DrugUse, data=PanelDataset, model="within", effect = "individual")
summary(Regression)

Regression2 = plm(Suicide_All ~  GDP_growth +
                    Depression + AlcoholUse+ Anxiety, data=PanelDataset, model="within", effect = "individual")
summary(Regression2)

#Sub-setting clusters for Regression 
K4 <- K4[,-3]
RegressionClusterData <- merge(x = FinalData, y = K4, by = "Country", all.x = TRUE)
K4ClustersReg <- split(RegressionClusterData,f = RegressionClusterData$Cluster)
K4ClustersReg[1]

for (j in 1:4) {
  PanelDatasetC4 <- pdata.frame(K4ClustersReg, index = c("Year","Country"))
}

PanelDatasetC4_1 <- pdata.frame(K4ClustersReg[[1]], index = c("Year","Country"))
PanelDatasetC4_2 <- pdata.frame(K4ClustersReg[[2]], index = c("Year","Country"))
PanelDatasetC4_3 <- pdata.frame(K4ClustersReg[[3]], index = c("Year","Country"))
PanelDatasetC4_4 <- pdata.frame(K4ClustersReg[[4]], index = c("Year","Country"))


#Regression on cluster 4
#Cluster4
RegressionC4 = plm(Suicide_All ~  GDP_growth +
                    Depression + AlcoholUse+ Anxiety, data=PanelDatasetC4_1, model="within", effect = "individual")
summary(RegressionC4)
#Cluster3
RegressionC3 = plm(Suicide_All ~  GDP_growth +
                    Depression + AlcoholUse+ Anxiety, data=PanelDatasetC4_2, model="within", effect = "individual")
summary(RegressionC3)
#Cluster2
RegressionC2 = plm(Suicide_All ~  GDP_growth +
                    Depression + AlcoholUse+ Anxiety, data=PanelDatasetC4_3, model="within", effect = "individual")
summary(RegressionC2)
#Cluster1
RegressionC1 = plm(Suicide_All ~  GDP_growth +
                    Depression + AlcoholUse+ Anxiety, data=PanelDatasetC4_4, model="within", effect = "individual")
summary(RegressionC1)





library(maps)
library(ggmap)


map.world <- map_data("world")



map.world_joined <- right_join(map.world, FinalData, by = c('region' = 'Country'))


#practivce
world_map <- map_data("world")

#graphs cluster 4
PanelDatasetC4_1_join <- right_join(map.world, PanelDatasetC4_1, by = c('region' = 'Country'))
#AlcoholUse
ggplot(PanelDatasetC4_1_join, aes(map_id= region, fill=AlcoholUse)) +
  geom_map(map = world_map, colour="black") +
  scale_fill_binned(type = "viridis")+
  expand_limits(x=PanelDatasetC4_1_join$long, y=PanelDatasetC4_1_join$lat)
#Depression
ggplot(PanelDatasetC4_1_join, aes(map_id= region, fill=Depression)) +
  geom_map(map = world_map, colour="black") +
  scale_fill_binned(type = "viridis")+
  expand_limits(x=PanelDatasetC4_1_join$long, y=PanelDatasetC4_1_join$lat)
#Anxiety
ggplot(PanelDatasetC4_1_join, aes(map_id= region, fill=Anxiety)) +
  geom_map(map = world_map, colour="black") +
  scale_fill_binned(type = "viridis")+
  expand_limits(x=PanelDatasetC4_1_join$long, y=PanelDatasetC4_1_join$lat)



#graphs cluster 3
PanelDatasetC4_2_join <- right_join(map.world, PanelDatasetC4_2, by = c('region' = 'Country'))
#GDP_growth
ggplot(PanelDatasetC4_2_join, aes(map_id= region, fill=GDP_growth)) +
  geom_map(map = world_map, colour="black") +
  scale_fill_binned(type = "viridis")+
  expand_limits(x=PanelDatasetC4_2_join$long, y=PanelDatasetC4_2_join$lat)
#AlcoholUse
ggplot(PanelDatasetC4_2_join, aes(map_id= region, fill=AlcoholUse)) +
  geom_map(map = world_map, colour="black") +
  scale_fill_binned(type = "viridis")+
  expand_limits(x=PanelDatasetC4_2_join$long, y=PanelDatasetC4_2_join$lat)



#graphs cluster 2
PanelDatasetC4_3_join <- right_join(map.world, PanelDatasetC4_3, by = c('region' = 'Country'))
#AlcoholUse
ggplot(PanelDatasetC4_3_join, aes(map_id= region, fill=AlcoholUse)) +
  geom_map(map = world_map, colour="black") +
  scale_fill_binned(type = "viridis")+
  expand_limits(x=PanelDatasetC4_3_join$long, y=PanelDatasetC4_3_join$lat)



#graphs cluster 1
PanelDatasetC4_4_join <- right_join(map.world, PanelDatasetC4_4, by = c('region' = 'Country'))
#Depression
PanelDatasetC4_4_join$Year <- as.factor(PanelDatasetC4_4_join)
ggplot(PanelDatasetC4_4_join, aes(map_id= region, fill=Depression)) +
  geom_map(map = world_map, colour="black") +
  scale_fill_binned(type = "viridis")+
  expand_limits(x=PanelDatasetC4_4_join$long, y=PanelDatasetC4_4_join$lat)+
  transition_states(
    Year,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink()

#Anxiety
ggplot(PanelDatasetC4_4_join, aes(map_id= region, fill=Anxiety)) +
  geom_map(map = world_map, colour="black") +
  scale_fill_binned(type = "viridis")+
  expand_limits(x=PanelDatasetC4_4_join$long, y=PanelDatasetC4_4_join$lat)






#Scatters for variables VS Suicide_All in .gifs

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
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
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
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
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
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
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
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
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
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
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
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
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
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
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
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
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
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
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
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  transition_states(
    Cluster,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() + 
  exit_shrink()














#devtools::install_github('thomasp85/gganimate')
#devtools::install_github("thomasp85/transformr")
#install_github("thomasp85/gganimate")
#build_github_devtools()

PanelDatasetC4_1_join$Year <- as.factor(PanelDatasetC4_1_join$Year)
ebola_plot_animate <- ggplot(data = PanelDatasetC4_1_join, aes(x = long, y = lat, group=group)) +
  geom_polygon(aes(fill = Suicide_All),color="black") +
  scale_fill_gradient(low = "#fcfbfd", high = "#2d004b") +
  coord_map() +
  theme_minimal()+
  transition_manual(Year)
## Plot data for the whole outbreak using gganimate() ##
ebola_plot_animate <- ggplot(data = ebola_data, aes(x = long, y = lat, group=group, frame=date.y)) +
  geom_polygon(aes(fill = cases),color="black") +
  scale_fill_gradient(low = "#fcfbfd", high = "#2d004b") +
  coord_map() +
  theme_minimal()

## animate plot with gganimate
gganimate(ebola_plot_animate, filename="ebola_gif.gif")







library(ggplot2)
library(gganimate)
library(gapminder)
theme_set(theme_bw())  # pre-set the bw theme.

PanelDatasetC4_1$Year = as.factor(PanelDatasetC4_1$Year)


ggplot(data= PanelDatasetC4_1_join, aes(AlcoholUse, Depression))+
  geom_point(aes(size = Suicide_all, color = Year, alpha=0.7))+
  geom_point(aes (size = Suicide_all) , color = "black", shape = 1)+
  #  geom_point(aes (size = Suicide_female) , color = Year, alpha=0.7, shape = 3))+
  scale_x_log10()+
  labs(
    x = "AlcoholUse",
    y = "Depression"
  )+
  theme_minimal() 
