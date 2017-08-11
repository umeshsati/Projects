library(xts)
library(readr)
library(dplyr)
library(forecast)


train <- read_csv("C:/Users/Administrator/Desktop/AV/train.csv")


test <- read_csv("C:/Users/Administrator/Desktop/AV/test.csv")


#sale_data$Datetime = as.character(sale_data$Datetime)

#sale_data <- train[,c("Datetime","Number_Of_Sales")]
##Sale <- xts(train$Number_Of_Sales,order.by=as.Date(sale_data$Datetime))
#####


train1 = train[,c("Price","Number_Of_Sales","Datetime","Item_ID")]


#[which(which(train1$Item_ID==i))]


##### init DF

submit <- data.frame(ID=integer(), Number_Of_Sales= double(),Price=double())

########### loop
for (i in (unique(test$Item_ID))){
  
  Preddata <- as.data.frame(train1[which(train1$Item_ID==i),])
  
  salets <- xts(Preddata$Number_Of_Sales,order.by=as.Date(Preddata$Datetime))
  pricets <- xts(Preddata$Price,order.by=as.Date(Preddata$Datetime))
  
  fit_sale <- auto.arima(salets)
  fit_price <- auto.arima(pricets)
  
  predlen =length(test$Datetime[which(test$Item_ID == i)])
  fc_sale <- forecast(fit_sale, h=predlen)
  fc_price <- forecast(fit_price, h=predlen)
  
  ######### appendddd
  data1 <- as.numeric(fc_price$mean)
  data2 <- as.numeric(fc_sale$mean)
  df <- as.data.frame(test$ID[which(test$Item_ID == i)])
  df <- cbind(df,as.data.frame(data2),as.data.frame(data1))
  names(df) <- c("ID","Number_Of_Sales","Price")
  
  submit <- as.data.frame(rbind(submit,df))
  names(submit) <- c("ID","Number_Of_Sales","Price")
}
