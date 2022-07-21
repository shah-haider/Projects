library(class)
library(gmodels)
library(rpart)
library(rpart.plot)
library(tree)
library(MASS)
library(ggplot2)

set.seed(52)

#KNN
# Scale numerical features
FinalData.x <- as.data.frame(scale(FinalData[,c(5,9,11,12,17)]))
head(FinalData.x)
summary(FinalData.x)
table(is.na(FinalData.x))

FinalData.x[["Suicide_All"]] <- ordered(cut(FinalData.x$Suicide_All, c(-1.4351 , -0.6944 , -0.2793 , 0.5102,
                                                                       + 3.8067)), labels = c("Extremely High", "High", "Low", "Extremely Low"))
# We have 519 observations. Lets do 65-35 split. 
.65*519

# Index
train <- sample(1:nrow(FinalData.x) , 337.35)
train
test<- -train
test

# Train and Test set
train.set <- FinalData.x[train,]
head(train.set)
test.set <- FinalData.x[test,]
head(test.set)

# Variables for Train and Test 
train.set.var<- train.set[,2:5] 
test.set.var <- test.set[, 2:5 ]

head(train.set.var)
head(test.set.var)

# Check to see sampling
prop.table(table(train.set$Suicide_All))

# Label Columns for Train and Test
train.label <- as.factor(train.set$Suicide_All)
train.label
test.label <- as.character(test.set$Suicide_All)
test.label
table(is.na(test.label))

#knn
# Loop for different values of k
nearest_1 <-as.character(knn(train=train.set.var,test=test.set.var, cl= train.label ,k=1))

table(test.label,nearest_1)

test.label
nearest_1
table(is.na(nearest_1))
test.label == nearest_1
# Accuracy
acc_1<- mean(test.label==nearest_1)
acc_1
acc<- c()

for (k in 1:15) {
  nearest_n <-knn(train=train.set.var, test=test.set.var, cl= train.label ,k=k)
  acc[k]<- mean(test.label==nearest_n)}
acc
CrossTable(x=test.label, y= nearest_n, prop.chisq = FALSE)

###############################PREDICT
Data2020 <- read_excel("2020DATA.xlsx")
TestSetP <- Data2020
TestSetP <- TestSetP[,-1]
TrainSetP <- FinalData.x
TrainLabelP <- TrainSetP[,1]
TrainSetP <- TrainSetP[,-1]

NearestP <-as.character(knn(train = TrainSetP, test = TestSetP, cl = TrainLabelP , k=1))
NearestP
TestSetP <- Data2020
Predictions <- cbind(TestSetP, NearestP)
names(Predictions)[6] <- "Predicted 2020 SuicideAllRates"

Toget2019<- as.data.frame(scale(FinalData[,c(5,9,11,12,17)]))
Toget2019 <- cbind(FinalData[,1:2] , Toget2019)
Toget2019[["Suicide_All"]] <- ordered(cut(Toget2019$Suicide_All, c(-1.4351 , -0.6944 , -0.2793 , 0.5102,
                                                                   + 3.8067)), labels = c("Extremely High", "High", "Low", "Extremely Low"))
Toget2019 <- Toget2019[,1:3]
Toget2019 <- split(Toget2019, f = Toget2019$Year)

O2016 <- Toget2019[[1]]
names(O2016)[3] <- "2016 Suicide Rates"
O2017 <- Toget2019[[2]]
names(O2017)[3] <- "2017 Suicide Rates"
O2018 <- Toget2019[[3]]
names(O2018)[3] <- "2018 Suicide Rates"
O2019 <- Toget2019[[4]]
names(O2019)[3] <- "2019 Suicide Rates"
remove(Toget2019)
O2016 <- O2016[,-2]
O2017 <- O2017[,-2]
O2018 <- O2018[,-2]
O2019 <- O2019[,-2]

Predictions <- merge(x=Predictions, y=O2016, by="Country", Keep.x = TRUE)
Predictions <- merge(x=Predictions, y=O2017, by="Country", Keep.x = TRUE)
Predictions <- merge(x=Predictions, y=O2018, by="Country", Keep.x = TRUE)
Predictions <- merge(x=Predictions, y=O2019, by="Country", Keep.x = TRUE)
Predictions <- Predictions[,-1]
View(Predictions)

prop.table(table(Predictions$`Predicted 2020 SuicideAllRates`))
prop.table(table(Predictions$`2016 Suicide Rates`))
prop.table(table(Predictions$`2017 Suicide Rates`))
prop.table(table(Predictions$`2018 Suicide Rates`))
prop.table(table(Predictions$`2019 Suicide Rates`))




##########################################################
#DECISION TREE

FinalData <- read.csv("FinalData.csv")
set.seed(51)
Data_tree <- subset(FinalData, FinalData$Year == 2019)
#Data_tree <- FinalData
Data_tree <- Data_tree[,c(-1,-4,-5,-7,-8,-11,-20)]
Data_tree <- Data_tree[,c(-1,-2)]
Data_tree[["Suicide_All"]] <- ordered(cut(Data_tree$Suicide_All, c(1.6 , 5.05 , 7.55 , 11.97,
                                                                   + 28.6)), labels = c("Extremely High", "High", "Low", "Extremely Low"))
str(Data_tree)
train = sort(sample(1:nrow(Data_tree),nrow(Data_tree)*0.65))
test <- -train

#Train data and Test data
training_data <- Data_tree[train,]
testing_data <- Data_tree[test,]

#Model:
tree <- rpart( Suicide_All~., data = training_data, method='class')
rpart.plot(tree,type=3,digits=3,fallen.leaves=TRUE, cex = 0.5)

#Accuracy
Tree_prediction = predict(tree, testing_data, type="class")
conf_mat_tree <- table(testing_data[,1], predicted = Tree_prediction )
sum(diag(conf_mat_tree))/sum(conf_mat_tree)

#Model 2:
tree <- rpart( Suicide_All~ GDP_growth + Depression + AlcoholUse + Anxiety, data = training_data, method='class')
rpart.plot(tree,type=3,digits=3,fallen.leaves=TRUE, cex = 0.5)

#Accuracy
Tree_prediction = predict(tree, testing_data, type="class")
conf_mat_tree <- table(testing_data[,1], predicted = Tree_prediction )
sum(diag(conf_mat_tree))/sum(conf_mat_tree)






