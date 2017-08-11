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


test$Datetime <- as.Date(test$Datetime)
test$year  = as.integer(format(test$Datetime, "%y"))
test$month <- as.integer(format(test$Datetime, "%m"))
test$day <- as.integer(format(test$Datetime, "%d"))


###### deal with the variables formating#######

sapply(train,class)
train <- train[!(names(train)%in%c("ID","Item_ID","Datetime"))]
table(is.na(train$Category_2))
train$Category_2[is.na(train$Category_2)] <- 999

ID = test$ID
test <- test[!(names(test)%in%c("ID","Item_ID","Datetime"))]
table(is.na(test$Category_2))
test$Category_2[is.na(test$Category_2)] <- 999

#################### narmality test

hist(train$Price)
qqnorm(train$Price)

hist(log(train$Number_Of_Sales))
qqnorm(log(train$Number_Of_Sales))

##### dummy variable #######
#train$Category_1 = as.factor(train$Category_1)
train$Category_2 = as.factor(train$Category_2)
train$Category_3 = as.factor(train$Category_3)


dmy <- dummyVars("~.",data = train[,1:2])

train_dummy <- data.frame(predict(dmy, newdata = train[,1:2]))

feature1 <- c("Category_1","year","month","day",colnames(train_dummy))

train <- as.data.frame(cbind(train,train_dummy))

test$Category_2 = as.factor(test$Category_2)
test$Category_3 = as.factor(test$Category_3)

dmytest <- dummyVars("~.",data = test[,2:3])
test_dummy <- data.frame(predict(dmytest, newdata = test[,2:3]))

test <- as.data.frame(cbind(test,test_dummy))



##### apply models ####


clf <- randomForest(train[,feature1], 
                    log(train$Price+1),
                    mtry=6,
                    
                    ntree=200,
                    sampsize=25000,
                    do.trace=TRUE)
clf$importance

plot(clf)

clf1 <- randomForest(train[,feature1], 
                     log(train$Number_Of_Sales+1),
                     mtry=6,
                     
                     ntree=200,
                     sampsize=25000,
                     do.trace=TRUE)
plot(clf1)


###### prediction


pred <- exp(predict(clf, test[,feature1]))-1

View(as.data.frame(pred))

pred1 <- exp(predict(clf1, test[,feature1]))-1

View(as.data.frame(pred1))



predictionts <- as.data.frame(cbind(ID,pred1,pred))

names(predictionts) <- c("ID","Number_Of_Sales","Price")
##### export to CSV
write.csv(predictionts,"C:/Users/Administrator/Desktop/AV/submission_RF_v4.csv",row.names = F)
