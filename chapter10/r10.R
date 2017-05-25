souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
tsSouvenir<- ts(souvenir, frequency=12, start=c(1987,1))

end(tsSouvenir)
dev.off()
trainData <- window(tsSouvenir, start=c(1987,1), frequency=12, end =c(1992,12))
testData <- window(tsSouvenir, start = c(1993,1), frequency=12)

library(forecast)
souvenirArima <- auto.arima(trainData)
souvenirForecast <- forecast.Arima(souvenirArima, h=12)
plot.forecast(souvenirForecast, 
              main = "Prognoza na przysz³e miesi¹ce", xlab = "Czas", ylab = "Sprzeda¿ upominków", lwd=3)

testData
souvenirForecast$mean
souvenirForecastErrors <- testData - souvenirForecast$mean
souvenirForecastErrors
mean(abs(souvenirForecastErrors))

library(forecast)
masefun <- function(observed, predicted){
  error = 0;
  if (length(observed) != length(predicted)) {
    return (NA);
  } else if (length(observed) == 0 || length(predicted) == 0) {
    return (NA);
  }
  else {
    denom = (sum(abs(observed[2:length(observed)] - observed[1:(length(observed) - 1)])))/(length(observed) - 1)
    error = sum((abs(observed-predicted)) / denom)/length(observed);
  }
  return (error);
}

smape <- function(observed, predicted){
  error = 0;
  if (length(observed) != length(predicted)) {
    return (NA);
  } else if (length(observed) == 0 || length(predicted) == 0) {
    return (NA);
  }
  else {
    error = sum((abs(observed-predicted)) / (observed+predicted))/length(observed);
  }
  return (100.0*error);
}

dataset1 <- maml.mapInputPort(1) # class: data.frame
time <- as.numeric(dataset1$Year)
observed_data <- as.numeric(dataset1$Immigration)
forecast <- as.numeric(dataset1$forecast)
plot(time,observed_data,type="l",col="blue",xlab="Time",ylab="Data",lwd=1.5)
lines(time,forecast,col="red",lwd=1.5)
legend("topleft",legend = c("Original Data","ARIMA Forecast"),bty=c("n","n"),lty=c(1,1),pch=16,col=c("blue","red"))

forecast_data_testwindow <- as.numeric(forecast[(which(!is.na(forecast)))])
actual_data_testwindow <- as.numeric(observed_data[(which(!is.na(forecast)))])
mase <- masefun(actual_data_testwindow,forecast_data_testwindow)
smape <- smape(actual_data_testwindow,forecast_data_testwindow)
arima_acc <- data.frame(Method=as.character("ARIMA"),accuracy(forecast_data_testwindow,actual_data_testwindow),MASE=mase,sMAPE=smape)
arima_acc$Method <- as.character(arima_acc$Method)
data.set <- arima_acc

lapply(data.set,class)
maml.mapOutputPort("data.set");


dataset1 <- maml.mapInputPort(1) # class: data.frame
data.set <- data.frame(Algorithm='Bayesian Regression')
data.set <- cbind(data.set, dataset1[1:6])
maml.mapOutputPort("data.set");

dataset1 <- maml.mapInputPort(1) # class: data.frame
data.set <- data.frame(Algorithm='NN Regression', 'Negative Log Likelihood'='NA', check.names=F)
data.set <- cbind(data.set, dataset1[1:5])
maml.mapOutputPort("data.set");
