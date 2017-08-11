library(readr)
library(caret)
library(randomForest)
library(xgboost)

##### import Dataset
train <- read_csv("C:/Users/Administrator/Desktop/AV/train.csv")
test <- read_csv("C:/Users/Administrator/Desktop/AV/test.csv")


####### work with Date features############

train$Datetime <- as.Date(train$Datetime)

train$year  = as.integer(format(train$Datetime, "%y"))
train$month <- as.integer(format(train$Datetime, "%m"))
train$day <- as.factor(format(train$Datetime, "%d"))
train$dow <- as.factor(weekdays(train$Datetime))

test$Datetime <- as.Date(test$Datetime)
test$year  = as.integer(format(test$Datetime, "%y"))
test$month <- as.integer(format(test$Datetime, "%m"))
test$day <- as.factor(format(test$Datetime, "%d"))
test$dow <- as.factor(weekdays(test$Datetime))


###### deal with the variables formating#######

sapply(train,class)
train <- train[!(names(train)%in%c("ID","Item_ID","Datetime"))]
table(is.na(train$Category_2))
train$Category_2[is.na(train$Category_2)] <- 999

ID = test$ID
test <- test[!(names(test)%in%c("ID","Item_ID","Datetime"))]
table(is.na(test$Category_2))
test$Category_2[is.na(test$Category_2)] <- 999


##### dummy variable #######
#train$Category_1 = as.factor(train$Category_1)
train$Category_2 = as.factor(train$Category_2)
train$Category_3 = as.factor(train$Category_3)

dmv <- c("Category_3","Category_2","day","dow")

dmy <- dummyVars("~.",data = train[dmv])

train_dummy <- data.frame(predict(dmy, newdata = train[dmv]))

feature1 <- c("Category_1","year","month",colnames(train_dummy))

train <- as.data.frame(cbind(train,train_dummy))

test$Category_2 = as.factor(test$Category_2)
test$Category_3 = as.factor(test$Category_3)

dmytest <- dummyVars("~.",data = test[dmv])

test_dummy <- data.frame(predict(dmytest, newdata = test[dmv]))

test <- as.data.frame(cbind(test,test_dummy))



##### apply models ####


 clf <- randomForest(train[,feature1], 
                    log(train$Price+1),
                    mtry=10,
                    nodesize=5,
                    ntree=100,
                    sampsize=20000,
                    do.trace=TRUE)
clf$importance

plot(clf)

clf1 <- randomForest(train[,feature1], 
                     log(train$Number_Of_Sales+1),
                     mtry=8,
                     nodesize = 3,
                     ntree=100,
                     sampsize=20000,
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
write.csv(predictionts,"C:/Users/Administrator/Desktop/AV/submission_RF_v3.csv",row.names = F)



############### XGB .1 73 12.46

'''train_set <- xgb.DMatrix(data.matrix(train[,feature1]),label= data.matrix(log(train$Price)) )
test_set <- xgb.DMatrix(data.matrix(test[,feature1]))

param = list(eta = 0.08, max_depth = 10,
             nround=2500,subsample = 0.5,colsample_bytree = 0.8,seed = 1,
             eval_metric = "rmse",objective = "reg:linear",min_child_weight=5,
             alpha=0.1, gama=0, lambda=0,early.stop.round=5 )
xgbm1 <- xgb.cv(params = param, data=train_set,nround=300, nfold = 5)

min(xgbm1$evaluation_log$test_rmse_mean)

xgbmodel1 <- xgb.train(params = param, data=train_set,nround=294, nfold = 5)


########### model for No of Sale
train_set1 <- xgb.DMatrix(data.matrix(train[,feature1]),label= data.matrix(log(train$Number_Of_Sales+1)) )

param1 = list(eta = 0.08, max_depth = 10,
              nround=2500,subsample = 0.5,colsample_bytree = 0.5,seed = 1,
              eval_metric = "rmse",objective = "reg:linear",min_child_weight=5,
              alpha=0.2, gama= 0, lambda=0 )
xgbm2 <- xgb.cv(params = param1, data=train_set1,nround=300, nfold = 5)

xgbmodel2 <- xgb.train(params = param1, data=train_set1,nround=300, nfold = 5)

###### prediction

predxgb <- exp(predict(xgbmodel1, test_set))

View(as.data.frame(predxgb))


predxgb1 <- exp(predict(xgbmodel2, test_set))-1

View(as.data.frame(predxgb1))


predictiontsxgb <- as.data.frame(cbind(ID,predxgb1,predxgb))

names(predictiontsxgb) <- c("ID","Number_Of_Sales","Price")
##### export to CSV
write.csv(predictiontsxgb,"C:/Users/Administrator/Desktop/AV/submission_xgb_v2.csv",row.names = F)


'''