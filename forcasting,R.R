### impoprt lib
library(readr)
library(forecast)

# impoprt Dataset
train <- read_csv("C:/Users/Administrator/Desktop/AV/train.csv")

#colnames(train)

#options(scipen = 999)
#train = train[,c("Datetime","Category_3","Category_2","Category_1","Price","Number_Of_Sales")]
a
#train$Datetime = as.Date(train$Datetime)


trainseries <- msts(train[,c("Datetime","Number_Of_Sales")], seasonal.periods=c(7,365.25))

plot(trainseries)
abline(reg=lm(trainseries~time(trainseries)),col=4)

#fit <- arima(diff(log(trainseries)),period =52 )


fit <- arima(trainseries[,2], order = c(0,1,1))
fc <- forecast(fit,5)
plot(fc)



pred <- predict(fit, n.ahead =26 )

View(as.data.frame(pred))
ts.plot(trainseries,2.718^pred$pred, log = "y", lty = c(1,3))
