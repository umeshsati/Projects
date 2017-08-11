library(xts)
library(readr)
library(dplyr)
library(forecast)


##### import Dataset
train <- read_csv("C:/Users/Administrator/Desktop/AV/train.csv")
test <- read_csv("C:/Users/Administrator/Desktop/AV/test.csv")

#### only required variables in account
train1 = train[,c("Price","Number_Of_Sales","Datetime","Item_ID")]


##### init empty DF######

submit <- data.frame(ID=integer(), Number_Of_Sales= double(),Price=double())

########### Model building ############
for (i in (unique(test$Item_ID))){
  
  Preddata <- as.data.frame(train1[which(train1$Item_ID==i),])

  salets <- xts(Preddata$Number_Of_Sales,order.by=as.Date(Preddata$Datetime))
  pricets <- xts(Preddata$Price,order.by=as.Date(Preddata$Datetime))
  
  fit_sale <- auto.arima(salets)
  fit_price <- auto.arima(pricets)
  
  predlen =length(test$Datetime[which(test$Item_ID == i)])
  fc_sale <- forecast(fit_sale, h=predlen)
  fc_price <- forecast(fit_price, h=predlen)
  
######### append DF#####
  
  data1 <- abs(as.numeric(fc_price$mean))
  data2 <- round(abs(as.numeric(fc_sale$mean)),0)
  df <- as.data.frame(test$ID[which(test$Item_ID == i)])
  df <- cbind(df,as.data.frame(data2),as.data.frame(data1))
  names(df) <- c("ID","Number_Of_Sales","Price")
  
  submit <- as.data.frame(rbind(submit,df))
  names(submit) <- c("ID","Number_Of_Sales","Price")
}

##### export to CSV
write.csv(submit,"C:/Users/Administrator/Desktop/AV/submission_v1.csv")

