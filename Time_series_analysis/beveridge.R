#Beveridge Wheat price index dataset PACF
library(tseries)
data(bev)


plot(bev)
# create a smoothing filter with simple moving average using 15 data points on either side
beveridge.ma = filter(bev, rep(1/31,31), sides=2)
lines(beveridge.ma, col='red')

par(mfrow=c(3,1))
#scale each data points by corresponding smoothed value
Y=bev/beveridge.ma
plot(Y)
#some data at beginning and end that has NA for Y
acf(na.omit(Y))
acf(na.omit(Y), type='partial')
#generate coefficients in an AR process
ar(na.omit(Y), order.max=5)

#MA(q) has ACF that cuts off after q lags
#AR(p) has PACF that cuts off after p lags

#estimate coefficients
ar(na.omit(Y), order.max = 5)