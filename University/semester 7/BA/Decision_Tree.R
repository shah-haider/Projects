

library(rpart)
library(rpart.plot)
library(tree)
library(MASS)
library(ggplot2)




getwd()
setwd("H:/7th semester/BA/grp prj")

FinalData <- read.csv("FinalData.csv")

#sub-setting

#FinalData <- subset(FinalData, Year == "2019")
#FinalData <- FinalData[,c(-1,-2,-3)]



#range(Suicide_All)

High <- ifelse(FinalData$Suicide_All >= 18, "Yes", "No")
High

testing_High <- High[test]



set.seed(42)
#View(FinalData)
#head(FinalData)
#dim(FinalData)
#names(FinalData)

# Splitting our data into appropriate TRAIN and TEST sets: 



#Lets predict whether Suicide  

#Index for train and test sets
train = sample(1:nrow(FinalData),nrow(FinalData)*0.65)#65-35 split between test and train
train
test <- -train
train
test

# Getting train data and test data by using the index from previous step:
training_data <- FinalData[train,]
testing_data <- FinalData[test,]



# Actual class 
testing_High <- High[test]

tree_model <- tree(High~., training_data) 
tree_model
plot(tree_model, type = "uniform")
text(tree_model, pretty = 0)

tree_pred <- predict(tree_model, testing_data, type = 'class')
tree_pred
mean(tree_pred != testing_High)
mean(tree_pred == testing_High)
table(testing_High, tree_pred)

#Prune the tree

#Cross validation to check where to stop pruning
set.seed(3)





#ErR: Tree is very Long
## Approach 1: rpart:
?rpart
# Build the model:
tree_FinalData <- rpart(FinalData$Suicide_All ~., data = training_data, method='anova')

tree_FinalData
?rpart.plot
rpart.plot(tree_FinalData,type=3,digits=3,fallen.leaves=TRUE)


#summary(tree.FinalData)


