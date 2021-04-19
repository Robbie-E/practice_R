# Modeling AR(4) process on JohnsonJohnson data
source('yule_walker.r')

# View the time series
par(mfrow=c(3,1))
plot(JohnsonJohnson)
acf(JohnsonJohnson)
pacf(JohnsonJohnson)

# Transform time series into a stationary dataset
par(mfrow=c(3,1))
plot(diff(log(JohnsonJohnson)))
acf(diff(log(JohnsonJohnson)))
pacf(diff(log(JohnsonJohnson)))

# Estimate parameters
phi = yule_walker(diff(log(JohnsonJohnson)), p=4)$phi
var = yule_walker(diff(log(JohnsonJohnson)), p=4)$var
cons = yule_walker(diff(log(JohnsonJohnson)), p=4)$cons