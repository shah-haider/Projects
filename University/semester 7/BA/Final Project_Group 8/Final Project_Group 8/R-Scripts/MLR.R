################################################################

#CORRELATION PLOT FOR REGRESSION
CorrData <- FinalData[,c(-1,-2,-19)]
corr <- cor(CorrData)
ggcorrplot(corr, hc.order = FALSE, lab_size = 3, type = "lower",
           lab = TRUE, title="Correlation Matrix for our variables",) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust=0.5, size=10)) +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.4, hjust=0.5, size=10)) 

PanelDataset <- pdata.frame(FinalData[,-19], index = c("Year","Country"))
pdim(PanelDataset)
summary(PanelDataset)

#Model 1 OF REGRESSION
##################################################################

#Fixed Effects
Regression = plm(Suicide_All ~ UP_all + GDP_growth +
                   Depression + AlcoholUse  + happiness + Schizophrenia +
                   Bipolar + Eating + Anxiety + DrugUse, data=PanelDataset, model="within", effect = "individual")

summary(Regression)

#Model 2 OF REGRESSION
Regression2 = plm(Suicide_All ~  GDP_growth +
                    Depression + AlcoholUse+ Anxiety + DrugUse, data=PanelDataset, model="within", effect = "individual")
summary(Regression2)

#Sub-setting clusters for Regression 
K4 <- K4[,-3]
RegressionClusterData <- merge(x = FinalData, y = K4, by = "Country", all.x = TRUE)
K4ClustersReg <- split(RegressionClusterData,f = RegressionClusterData$Cluster)
K4ClustersReg[1]

PanelDatasetC4_1 <- pdata.frame(K4ClustersReg[[1]], index = c("Year","Country"))
PanelDatasetC4_2 <- pdata.frame(K4ClustersReg[[2]], index = c("Year","Country"))
PanelDatasetC4_3 <- pdata.frame(K4ClustersReg[[3]], index = c("Year","Country"))
PanelDatasetC4_4 <- pdata.frame(K4ClustersReg[[4]], index = c("Year","Country"))

#Regression on cluster 1
#Cluster with middle eastern countries 
RegressionC4 = plm(Suicide_All ~  GDP_growth +
                     Depression + AlcoholUse+ Anxiety, data=PanelDatasetC4_1, model="within", effect = "individual")
summary(RegressionC4)

#Regression on cluster 2
#Cluster with developed countries
RegressionC3 = plm(Suicide_All ~  GDP_growth +
                     Depression + AlcoholUse+ Anxiety, data=PanelDatasetC4_2, model="within", effect = "individual")
summary(RegressionC3)

#Regression on cluster 3
#Cluster with under developed or developing countries 
RegressionC2 = plm(Suicide_All ~  GDP_growth +
                     Depression + AlcoholUse+ Anxiety, data=PanelDatasetC4_3, model="within", effect = "individual")
summary(RegressionC2)

#Regression on cluster 4
#Cluster4
RegressionC1 = plm(Suicide_All ~  GDP_growth +
                     Depression + AlcoholUse+ Anxiety, data=PanelDatasetC4_4, model="within", effect = "individual")
summary(RegressionC1)


#MAPPING THE SIGNIFICANT VARIABLE FOR REGRESSION
library(maps)


map.world <- map_data("world")

map.world_joined <- right_join(map.world, FinalData, by = c('region' = 'Country'))

FinalData$Country <- recode(FinalData$Country
                            ,'United States' = 'USA'
                            ,'United Kingdom' = 'UK'
                            ,'Egypt, Arab Rep.' = 'Egypt'
                            ,'Russian Federation' = 'Russia'
                            ,'Korea, Rep.' = 'South Korea'
                            ,'Iran, Islamic Rep.' = 'Iran'
                            ,'Congo' = 'Democratic Republic of the Congo'
)


#graphs cluster 1
PanelDatasetC4_1_join <- right_join(map.world, PanelDatasetC4_1, by = c('region' = 'Country'))
#AlcoholUse
ggplot(PanelDatasetC4_1_join, aes(map_id= region, fill=AlcoholUse)) +
  geom_map(map = map.world, colour="black") +
  scale_fill_binned(type = "viridis")+
  expand_limits(x=PanelDatasetC4_1_join$long, y=PanelDatasetC4_1_join$lat)
#Depression
ggplot(PanelDatasetC4_1_join, aes(map_id= region, fill=Depression)) +
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

#GDP_growth
ggplot(PanelDatasetC4_2_join, aes(map_id= region, fill=GDP_growth)) +
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
