library(readr)
library(dplyr)
library(xgboost)

##### import Dataset
train <- read_csv("C:/Users/Administrator/Desktop/AV/train.csv")
test <- read_csv("C:/Users/Administrator/Desktop/AV/test.csv")


####### work with Date features############

train$Datetime <- as.Date(train$Datetime)

train$year  = as.integer(format(train$Datetime, "%y"))
train$month <- as.integer(format(train$Datetime, "%m"))
train$day <- as.integer(format(train$Datetime, "%d"))


test$Datetime <- as.Date(test$Datetime)
test$year  = as.integer(format(test$Datetime, "%y"))
test$month <- as.integer(format(test$Datetime, "%m"))
test$day <- as.integer(format(test$Datetime, "%d"))


###### deal with the variables formating#######

sapply(trainset,class)
trainset <- train[!(names(train)%in%c("ID","Item_ID","Datetime"))]
table(is.na(trainset$Category_2))
trainset$Category_2[is.na(trainset$Category_2)] <- 999


testset <- test[!(names(test)%in%c("ID","Item_ID","Datetime"))]
table(is.na(testset$Category_2))
testset$Category_2[is.na(testset$Category_2)] <- 999

#################### narmality test

hist(train$Price)
qqnorm(train$Price)

hist(log(train$Number_Of_Sales))
qqnorm(log(train$Number_Of_Sales))



##### apply models ####
feature1 <- c("Category_3","Category_2","Category_1","year","month","day")

clf <- randomForest(trainset[,feature1], 
                    trainset$Price,
                    mtry=5,
                    ntree=100,
                    sampsize=20000,
                    do.trace=TRUE)
clf$importance

plot(clf)

clf1 <- randomForest(trainset[,feature1], 
                     log(trainset$Number_Of_Sales+1),
                     mtry=5,
                     ntree=100,
                     sampsize=15000,
                     do.trace=TRUE)
plot(clf1)


###### prediction



pred <- predict(clf, testset[,feature1])

View(as.data.frame(pred))

pred1 <- exp(predict(clf1, testset[,feature1]))-1

View(as.data.frame(pred1))


ID = test$ID

predictionts <- as.data.frame(cbind(ID,pred1,pred))

names(predictionts) <- c("ID","Number_Of_Sales","Price")
##### export to CSV
write.csv(predictionts,"C:/Users/Administrator/Desktop/AV/submission_RF.csv")

