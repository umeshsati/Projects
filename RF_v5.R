library(readr)
library(caret)
library(randomForest)

##### import Dataset
train <- read_csv("C:/Users/Administrator/Desktop/AV/train.csv")
test <- read_csv("C:/Users/Administrator/Desktop/AV/test.csv")


####### work with Date features############

train$Datetime <- as.Date(train$Datetime)

train$year  = as.integer(format(train$Datetime, "%y"))
train$month <- as.integer(format(train$Datetime, "%m"))
train$day <- as.integer(format(train$Datetime, "%d"))
train$dow <- as.integer(factor(weekdays(train$Datetime)))


test$Datetime <- as.Date(test$Datetime)
test$year  = as.integer(format(test$Datetime, "%y"))
test$month <- as.integer(format(test$Datetime, "%m"))
test$day <- as.integer(format(test$Datetime, "%d"))
test$dow <- as.integer(factor(weekdays(test$Datetime)))

###########  plot data against year#####

#plot(train$Datetime, train$Price)

#plot(train$Datetime, train$Number_Of_Sales)

#train <- train[which(train$year>14),]

######outlier####

#quantile(train$Price,.9998)

#train <- train[which(train$Price < 306),]
###### deal with the variables formating#######

sapply(train,class)
train <- train[!(names(train)%in%c("ID","Item_ID","Datetime"))]
table(is.na(train$Category_2))
train$Category_2[is.na(train$Category_2)] <- 999

ID = test$ID
test <- test[!(names(test)%in%c("ID","Item_ID","Datetime"))]
table(is.na(test$Category_2))
test$Category_2[is.na(test$Category_2)] <- 999
'''
##### dummy variable #######
train$Category_1 = scale(train$Category_1)
train$Category_2 = as.factor(train$Category_2)
train$Category_3 = as.factor(train$Category_3)


dmy <- dummyVars("~.",data = train[,1:2])

train_dummy <- data.frame(predict(dmy, newdata = train[,1:2]))

feature1 <- c("Category_1","year","month","day","dow",colnames(train_dummy))

train <- as.data.frame(cbind(train,train_dummy))

test$Category_2 = as.factor(test$Category_2)
test$Category_3 = as.factor(test$Category_3)

dmytest <- dummyVars("~.",data = test[,2:3])
test_dummy <- data.frame(predict(dmytest, newdata = test[,2:3]))

test <- as.data.frame(cbind(test,test_dummy))'''
feature1 <- c("Category_1","Category_2","Category_3","year","month","day","dow")

###### Model Selection #####


clf <- randomForest(train[,feature1], 
                    train$Price,
                    mtry=3,
                    ntree=150,
                    sampsize=20000,
                    do.trace=TRUE)
clf$importance

plot(clf)

clf1 <- randomForest(train[,feature1], 
                     train$Number_Of_Sales,
                     mtry=3,
                     ntree=150,
                     sampsize=20000,
                     do.trace=TRUE)
plot(clf1)

options(scipen = 999)
###### prediction


pred <- predict(clf, test[,feature1])

View(as.data.frame(pred))

pred1 <- predict(clf1, test[,feature1])

View(as.data.frame(pred1))



predictionts <- as.data.frame(cbind(ID,pred1,pred))

names(predictionts) <- c("ID","Number_Of_Sales","Price")
##### export to CSV
write.csv(predictionts,"C:/Users/Administrator/Desktop/AV/submission_RF_v7.csv",row.names = F)



