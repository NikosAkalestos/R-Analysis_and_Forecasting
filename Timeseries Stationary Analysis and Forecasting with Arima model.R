library(readxl)
TS_GDP <- read_excel("TS_GDP.xlsx")
View(TS_GDP)

# CHOOSE GREECE
tsGR<-TS_GDP[,c("Greece")]

# Transform it in a time series 
tsGR<-ts(tsGR)

# plot
ts.plot(tsGR)

# Is this stationary? Check ACF and PACF
acf(tsGR)
pacf(tsGR)

# CREATE FIRST DIFFERENCES AND CHECK ACF PACF
td1<-diff(tsGR,lag=1,differences = 1)
plot(td1, main="First differences of Greece GDP", ylab="First differences")

# Do second differences
td2<-diff(tsGR,lag=1,differences = 2)
plot(td2, main="Second differences of Greece GDP", ylab="Second differences")

acf(td1, main="ACF of first differences of GDP", ylab="ACF")
pacf(td1, main="PACF of first differences of GDP", ylab="PACF")
acf(td2, main="ACF of second differences of GDP", ylab="ACF")
pacf(td2, main="PACF of second differences of GDP", ylab="PACF")

# INSTALL PACKAGES FOR AUTO.ARIMA
install.packages("xts")
install.packages("forecast", dependencies = TRUE)
library(forecast)


model1<-arima(tsGR, order=c(0,0,0))
model1
coeftest(model1) # Tick lmtest package in "Packages"
checkresiduals(model1) # Tick forecast package in "Packages"

model2<-arima(tsGR, order=c(1,0,0))
model2
coeftest(model2)
checkresiduals(model2)

model3<-arima(tsGR, order=c(2,0,0))
model3
coeftest(model3)
checkresiduals(model3)

model4<-arima(tsGR, order=c(1,1,0))
model4
coeftest(model4)
checkresiduals(model4)

model5<-arima(tsGR, order=c(2,1,0))
model5
coeftest(model5)
checkresiduals(model5)

model6<-auto.arima(tsGR)
model6
coeftest(model6)
checkresiduals(model6)

f1<-forecast(model2,h=10)
f2<-forecast(nodel6,h=10)

plot(f1)
plot(f2)
accuracy(f1)
accuracy(f2)
