library(readxl)
a<-read_excel(file.choose())

b <- ts(a$A, start=c(2013,1),end=c(2019,10), freq=12)
plot(b)

##find autocorrelation using durbin watson test
library(zoo)
library(lmtest)

z<-na.omit(a)
y=z$I
dwtest(y~.,data=z)

"14% there is no correlation since u have 86% p value "

##to find stationary using dickey fuller
tseries::adf.test(b)

"there is no stationary since p value is greater than 0.05 so we go for differial series(taking log and differntiation)"

##acf
acf(b)

##pacf
pacf(b)

##finding d value
plot((log(b)))
plot(diff(log(b))) 

##using arima
library(forecast)
auto.arima(b)
fit=arima(b,c(1,1,0),seasonal=list(order=c(1,1,0), period=12))
fit
"from fit, we see aic and change the p  or q value the value aic which is lesser shd be taken and we shd include that that p,d,q value "
coeftest(fit)
" using p value if it is not significant then we have to put 0 i.e in this model moving avg error is not significant so in seasonal i have given 0 instead of 1"
fitted(fit)
"in this case we used last three months as fitted value i.e from aug-oct 2019 "

##forcast the last three month using mean absolute error(accuracy)
fitted_val<- c(37529.42,31787.55,40458.61)
actual<-c(31458,34102,39547) 
"actual is given data points from aug-sep 2019"
mean((abs(actual-fitted_val))/actual)
"therefore there is only 9% error in the data ,accuracy of the data=91%"
plot(actual)
lines(fitted_val)

##plot the forecast
library(ggplot2)
library(forecast)
forcasted_value<-forecast(fit)
forcasted_value
" now we forcasted value for nov,dec etc"
plot(x)
"there is wide range in forecast i.e grey area which says there is more flutuation and u r ot adviced to invest it may go up or down only if the grey area is narrow u have confidence to invest"






