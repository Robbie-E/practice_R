# Simulating ARIMA(2,1,1) Process
phi=c(.7, .2); beta=0.5; sigma=3; m=10000
set.seed(42)
Simulated.Arima=arima.sim(n=m,list(order = c(2,1,1), ar = phi, ma=beta))

par(mfrow=c(2,1))
plot(Simulated.Arima, ylab=' ',main='Simulated time series from ARIMA(2,1,1) process', col='blue', lwd=2)
acf(Simulated.Arima)

par(mfrow=c(3,1))
Diff.Simulated.Arima=diff(Simulated.Arima)
plot(Diff.Simulated.Arima)
acf(Diff.Simulated.Arima)
pacf(Diff.Simulated.Arima)

library(astsa)
sarima(Simulated.Arima,2,1,1,0,0,0)

library(forecast)
#generates plot of residuals, ACF, PACF and p-values
auto.arima(Simulated.Arima)

fit1<-arima(Diff.Simulated.Arima, order=c(4,0,0))
fit2<-arima(Diff.Simulated.Arima, order=c(2,0,1))
fit3<-arima(Simulated.Arima, order=c(2,1,1))

# Using Q statistic
#Test H0: several acf coefficients are zero
#Box.test(data, lag=log(T))