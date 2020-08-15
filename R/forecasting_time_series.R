#Forecasting time seris using R by Prof Rob J Hyndman

library(fpp)

#three examples of simple forecasting models
beer <- aggregate(ausbeer)
plot(beer) #annual beer production in Australia
plot(a10) #sales of pharma products in Australia, of group A10 pharma products
plot(taylor) #half hourly electricity demands 

#Fully automated forecasting
plot(forecast(beer)) #shows 80% and 95% prediction intervals and the trendline
plot(forecast(a10))
plot(forecast(taylor))

#test methods on a test set
beertrain <- window(beer, end = 1999.99)
beertest <- window(beer, start = 2000)
a10train <- window(a10, end = 2005.99)
a10test <- window(a10,start = 2006)

#simple methods for the BEER data
f1 <- meanf(beertrain, h = 8) #mean method
f2 <- rwf(beertrain, h = 8) #naive method
f3 <- rwf(beertrain, drift=TRUE, h =8) #drift method

plot(f2)

#In-sample accuracy
accuracy(f1)
accuracy(f2)
accuracy(f3)

#Out of sample accuracy
accuracy(f1, beertest)
accuracy(f2, beertest)
accuracy(f3, beertest)

#exponential smoothing
fit1 <- ets(beertrain, model = "ANN", damped = FALSE)
fit2 <- ets(beertrain)

fcast1 <- forecast(fit1, h = 8)
fcast2 <- forecast(fit2, h = 8)

plot(fcast2)
