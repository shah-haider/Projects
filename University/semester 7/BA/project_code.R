library(readxl)
library(reshape2)
library(dplyr)
library(ExPanDaR)
library(NbClust)
library(ggplot2)
library(factoextra)

getwd()
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

write.csv(FinalData, "FinalData.csv")

ExPanD(df = FinalData, cs_id = "Country", ts_id = "Year" )

###
HDI <- distinct(HDI)
###

#HClust
Data2019 <- subset(FinalData, Year == "2019")

DM_Data <- Data2019[,c(-1,-2,-19)]

DM_Data <- scale(DM_Data)
DM_Data <- as.data.frame(DM_Data)
DM <- dist(as.matrix(DM_Data))
AvgHC <-hclust(DM, method="average")
plot(AvgHC, hang = -1, cex=0.8 ,labels = Data2019$Country)

#g24 <- cutree(AvgHC, k = c(2,4))
#table(grp2 = g24[,"2"], grp4 = g24[,"4"])

#7clusters
clustCut7<-cutree(AvgHC,k=10)
clustCut7
ta = table(clustCut7)
ta
o=order(clustCut7)
o
clustCut7 <- data.frame(Data2019$Country[o], clustCut7[o])
clustCut7
colnames(clustCut7) <- c("Country", "Cluster")
colnames(clustCut7)
rect.hclust(AvgHC, k=10, border="blue")



clustCut7 <- merge(x = clustCut7, y = HDI , by = "Country", all = FALSE)
#fviz_cluster(list(data=Data2019, cluster= clustCut7), geom = "point")

fviz_cluster(list(data=DM_Data, cluster= clustCut7))+geom_text(label=clustCut7$Country, size = 2, vjust=0)

#Optimal
Nb<- NbClust(DM_Data, distance = "euclidean", min.nc = 2, max.nc=10, method = "average", index ="all")
fviz_nbclust(Nb) 

#KMeans
set.seed(51)
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