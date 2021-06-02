#Simulate ARMA mixed process
#X[t] = 0.7 X[t-1] + Z[t] + 0.2 Z[t-1]
set.seed(42)
data = arima.sim(list(order = c(1,0,1), ar=.7, ma=.2), n = 1000000)
par(mfcol=c(3,1))
plot(data, xlim=c(0,400))
acf(data)
acf(data, type="partial")

#Use auto.arima from forecast library
library(forecast)
data1 = arima.sim(n=1E5, list(ar=.5, ma=.2))
par(mfcol=c(3,1))
plot(data1)
acf(data1)
acf(data1, type="partial")
print(auto.arima(data1))