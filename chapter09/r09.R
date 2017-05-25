
rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
tsRain <- ts(rain,start=c(1813))
plot(tsRain, ann=FALSE, lwd=2)
title(main = "Opady w Londynie", xlab = "Lata", ylab = "Opad w calach")

rainModel <- HoltWinters(tsRain, beta=FALSE, gamma=FALSE, l.start=23.56)
rainModel
rainModel$fitted
plot(rainModel, main = "Prognoza dla historycznych danych", xlab = "Lata", ylab = "Wartoœci faktyczne versus prognozowane", lwd=3)

library(forecast)
rainForecasts <- forecast.HoltWinters(rainModel, h=7)
rainForecasts
plot(rainForecasts, main = "Prognoza na przysz³e lata", xlab = "Lata", ylab = "Opad w calach", lwd=2)

skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
tsSkirts <- ts(skirts,start=c(1866))
skirtsModel <- HoltWinters(tsSkirts, gamma=FALSE)
skirtsModel
plot(skirtsModel, main = "Prognoza dla historycznych danych", xlab = "Lata", ylab = "Wartoœci faktyczne versus prognozowane", lwd=3)

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
tsBirths <- ts(births, frequency=12, start=c(1946,1))
plot(tsBirths, ann=FALSE, lwd=2)
title(main = "Liczba urodzeñ w Nowym Jorku", xlab = "Czas", ylab = "Liczba urodzeñ")

dev.off()
birthsModel <- HoltWinters(tsBirths)
birthsModel
plot(birthsModel, main = "Progoza dla historycznych danych", xlab = "Czas", ylab = "Wartoœci faktyczne versus prognozowane", lwd=3)

birthsForecasts <- forecast.HoltWinters(birthsModel, h=24)
plot(birthsForecasts, main = "Prognoza na przysz³e miesi¹ce", xlab = "Czas", ylab = "Liczba urodzeñ", lwd=2)

tsdisplay(residuals(birthsForecasts), main = "B³êdy prognoz")

?arima.sim
simAr<-arima.sim(list(ar=c(0.4,0.4)),n=1000)
simMa<-arima.sim(list(ma=c(0.6,-0.4)),n=1000)
par(mfrow=c(3,2))
plot(simAr, main="Symulacja modelu autoregresyjnego AR(2)")
plot(simMa, main="Symulacja modelu œredniej krocz¹cej MA(2)")
acf(simAr,main="Wspó³czynnik ACF modelu AR(2)")
acf(simMa,main="Wspó³czynnik ACF modelu MA(2)")
pacf(simAr,main="Wspó³czynnik PACF modelu AR(2)")
pacf(simMa,main="Wspó³czynnik PACF modelu MA(2)")

par(mfrow=c(2,2))
tsSkirtsDiff1 <- diff(tsSkirts, differences=1)
tsSkirtsDiff2 <- diff(tsSkirts, differences=2)
tsSkirtsDiff3 <- diff(tsSkirts, differences=3)
tsSkirtsDiff4 <- diff(tsSkirts, differences=4)
plot (tsSkirts, main="Oryginalny szereg czasowy", lwd=2)
plot(tsSkirtsDiff1, main="Ró¿nicowanie pierwszego rzêdu", lwd=2)
plot(tsSkirtsDiff2, main="Ró¿nicowanie drugiego rzêdu", lwd=2)
plot(tsSkirtsDiff3, main="Ró¿nicowanie trzeciego rzêdu", lwd=2)


par(mfrow=c(2,1))
?acf
library(forecast)
acf(tsSkirtsDiff2, xlab = "OpóŸnienie")
pacf(tsSkirtsDiff2, xlab = "OpóŸnienie", ylab = "PACF")

auto.arima(tsSkirts)

skirtsModel <- arima(tsSkirts, order=c(1,2,0))
skirtsForecast <-forecast.Arima(skirtsModel, h=10)
skirtsForecast

nn <- nnetar(tsBirths)
nn

nn1 <- nnetar(tsBirths, size=1)

L <- BoxCox.lambda(ts(tsBirths, frequency=12), method="loglik")
nnBC <- nnetar(tsBirths,lambda=L, size=3)

par(mfrow=c(3,2))
plot(tsBirths, lwd=2, xlab = "Czas", ylab = "Liczba urodzeñ", main = "Dane i prognozy historyczne automatycznie skonfigurowanej sieci neuronowej")
lines(nn$fitted,col="red",lty=3, lwd=3)
plot(forecast(nn,h=60),lwd=2, xlab = "Czas", ylab = "Liczba urodzeñ", main = "Dane uzupe³nione o prognozy automatycznie skonfigurowanej sieci neuronowej")
plot(tsBirths, lwd=2, xlab = "Czas", ylab = "Liczba urodzeñ", main = "Dane i prognozy historyczne sieci neuronowej z 1 ukrytym wêz³em")
lines(nn1$fitted,col="red", lty=3, lwd=3)
plot(forecast(nn1,h=60),lwd=2, xlab = "Czas", ylab = "Liczba urodzeñ", main = "Dane uzupe³nione o prognozy sieci neuronowej z 1 ukrytym wêz³em")
plot(tsBirths, lwd=2, xlab = "Czas", ylab = "Liczba urodzeñ", main = "Dane i prognozy historyczne sieci neuronowej nauczonej na przekszta³conych danych")
lines(nn1$fitted,col="red", lty=3, lwd=3)
plot(forecast(nn1,h=60),lwd=2, xlab = "Czas", ylab = "Liczba urodzeñ", main = "Dane uzupe³nione o prognozy sieci neuronowej nauczonej na przekszta³conych danych")

