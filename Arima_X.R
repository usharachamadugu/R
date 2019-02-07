library(fpp2)
library(tseries)
library(ggplot2)
data("elecsales")
autoplot(elecequip)

adf.test(elecequip, alternative = "stationary")

?adf.test
elecqeuipdiff1 = diff(elecequip, 1)

autoplot(elecqeuipdiff1)

adf.test(elecqeuipdiff1, alternative = "stationary" )

### decide the d = 1 

### pacf and acf plots 

acf(elecqeuipdiff1)
pacf(elecqeuipdiff1)

### auto.arima 

auto.arima(elecequip)


### Arima 
elec_Ar = Arima(elecequip,order =  c(3,1,4), seasonal = c(0,1,1))

## Window the ttrain and test data 

electrain = window(elecequip, end= c(2011,3))
electest = window(elecequip, start=c(2011,4))

### Arima model 

elec_Ar = Arima(electrain, order = c(3,1,4), seasonal = c(0,1,1))
elec_Ar

elec_Ar = Arima(electrain, order = c(4,1,4), seasonal = c(0,1,1))
elec_Ar

## performance of the Arima model 

elecforecast = forecast(elec_Ar, h=12)

### Mape 

err = electest - elecforecast$mean

mape = mean((abs(err)/electest)*100)
mape

checkresiduals(elecforecast)


### 

autoplot(AirPassengers)
AirPassengers

## Holtwinters model 
airdecomp = decompose(AirPassengers)
autoplot(airdecomp)

### create train and test 

airtrain = window(AirPassengers, end = c(1959,12))

airtest = window(AirPassengers, start=c(1960,1))

### holtwinters with alpha, beta , gamma and "additive" 

airhw_add = HoltWinters(airtrain, seasonal = "additive")

### 
plot(airhw_add)

## MOdel performance using Mape 

airforec = forecast(airhw_add, h=12)

err = airtest - airforec$mean

mape = mean((abs(err)/airtest)*100)
mape
### hotlwinters multiplicative model 
airhw_mul = HoltWinters(airtrain, seasonal = "multiplicative")

plot(airhw_mul)

airforec = forecast(airhw_mul, h=12)

err = airtest - airforec$mean
mape = mean((abs(err)/airtest)*100)
mape

### Arima 

airdiff1 = diff(AirPassengers, 1)
plot(airdiff1)

airdiff2 = diff(AirPassengers, 2)
plot(airdiff2)

airbox = BoxCox(AirPassengers, lambda = "auto")
plot(airbox)

?BoxCox

airbox

### rainfall timeseries data 
setwd("D:\\AP\\Forecasting")

rainfall = read.csv("Chennai_rainfall.csv")

head(rainfall)

rainseries = ts(rainfall$Rain, start=c(2000,1), frequency = 12)

autoplot(rainseries)


### seasionality 

raindecomp = decompose(rainseries)
plot(raindecomp)

## Hotwinters model 

raintrain = window(rainseries, end = c(2016,7))
raintest = window(rainseries, start=c(2016,8))

### 
rainhw = HoltWinters(raintrain, seasonal = "additive")
plot(rainhw)

## rainhwmul = HoltWinters(raintrain, seasonal = "multiplicative")

##performance of holtwinters model 

rainforec = forecast(rainhw, h=12)

err = raintest - rainforec$mean

rmse = sqrt(mean(err**2))
rmse

### Arima model 

raindiff1 = diff(rainseries,1)
plot(raindiff1)

raindiff2 = diff(rainseries,2)
plot(raindiff2)

### adftest 
adf.test(raindiff2)

### d = 2 

pacf(raintrain)
acf(raintrain)

### arima(0,2,1)

rain_Arima = Arima(raintrain, order=c(1,2,2), seasonal = c(0,2,2))

### forecast by rainarima 

rainforec = forecast(rain_Arima, h=12)

err = raintest - rainforec$mean

rmse = sqrt(mean(err**2))
rmse

checkresiduals(rain_Arima)


### Arimax model 

icecream = read.csv("Icecream.csv")

iceseries = ts(icecream$cons, start=c(2015,1), frequency = 12)

autoplot(iceseries)

## decompose 

icedecomp = decompose(iceseries)
plot(icedecomp)


### auto arima model o ice series 

icetrain = window(iceseries, end = c(2016,12))
icetest = window(iceseries, start = c(2017,1))
icearima = auto.arima(icetrain, seasonal = F)
icearima

pacf(icetrain)
acf(icetrain)
auto.arima(iceseries)

### ts 

ice_ar = Arima(icetrain, order=c(1,0,0))
ice_ar

### Model performance 

forec = forecast(ice_ar, h=6)

err = icetest - forec$mean

mape = mean((abs(err)/icetest)*100)
mape

#### Arimax 

vars = icecream$temp[1:24]

ice_ar = Arima(icetrain, order=c(1,0,1), xreg = vars)

var = icecream$temp[25:30]

forec = forecast(ice_ar,xreg = var)

err = icetest - forec$mean

mape = mean((abs(err)/icetest)*100)
mape

## include both temp and income 
vars2 = icecream[1:24,c(3,5)]
vars2
length(vars)
length(vars2)
ice_ar = Arima(icetrain, order=c(1,0,1), xreg = vars2)


### forecast using both income and temp 
vars = icecream[25:30,c(3,5)]
forec = forecast(ice_ar, xreg=vars)

## model performance 

err = icetest - forec$mean

mape = mean((abs(err)/icetest)*100)
mape

## include both temp and income 
vars2 = icecream[1:24,c(4,5)]
vars2
length(vars)
length(vars2)
ice_ar = Arima(icetrain, order=c(1,0,1), xreg = vars2)


### forecast using both income and temp 
vars = icecream[25:30,c(4,5)]
forec = forecast(ice_ar, xreg=vars)

## model performance 

err = icetest - forec$mean

mape = mean((abs(err)/icetest)*100)
mape


#### Arimax model on chennai rainfall data 

rain = rainfall[1:137,]
tail(rain)

#### convert rain to a timeseries 

rainseries = ts(rain$Rain, start = c(2001,1), frequency = 12)
plot(rainseries)
length(rainseries)
raintrain = window(rainseries, end=c(2010,6))
raintest = window(rainseries, start=c(2010,7))
length(raintest)
length(raintrai)
### Arima model 

rain_ar = auto.arima(raintrain, seasonal = T)
rain_ar

## model performance using RMSE 

forec = forecast(rain_ar, h=12)

err = raintest - forec$mean

rmse = sqrt(mean(err**2))
rmse

### Arimax model 
names(rain)
vars = rain$IOD[1:114]
length(vars)
length(raintrain)

rain_ar = auto.arima(raintrain, seasonal = T, xreg=vars)

## Model performance with IOD as input variable 
var = rain$IOD[115:(115+11)]
length(var)
forec = forecast(rain_ar, h=12, xreg = var)

### model performance 

err = raintest - forec$mean

err = raintest - forec$mean

rmse = sqrt(mean(err**2))
rmse


## include IOD and QBO 
names(rain)
vars = rain[1:114,6:7 ]

rain_ar = auto.arima(raintrain, seasonal = T, xreg = vars)

var = rain[115:(115+11),6:7 ]

forec = forecast(rain_ar, h=12, xreg = var)

err = raintest - forec$mean

rmse = sqrt(mean(err**2))
rmse

checkresiduals(rain_ar)

forec$mean

plot(forec)
