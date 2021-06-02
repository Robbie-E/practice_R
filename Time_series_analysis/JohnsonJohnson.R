# Modeling AR(4) process on JohnsonJohnson data
source('yule_walker.r')

# View the time series
par(mfrow=c(3,1))
plot(JohnsonJohnson)
acf(JohnsonJohnson)
pacf(JohnsonJohnson)

# Transform time series into a stationary dataset
# log return model: r[t] = log X[t] - log X[t-1]
par(mfrow=c(3,1))
plot(diff(log(JohnsonJohnson)))
acf(diff(log(JohnsonJohnson)))
pacf(diff(log(JohnsonJohnson)))

# Estimate parameters
phi = yule_walker(diff(log(JohnsonJohnson)), p=4)$phi
var = yule_walker(diff(log(JohnsonJohnson)), p=4)$var
cons = yule_walker(diff(log(JohnsonJohnson)), p=4)$cons

# r[t] = cons + phi (dot) r[t-1, ..., t-4] + Z[t]
# Z[t] ~ N(0,var)
#> phi
#           [,1]
#[1,] -0.6293492
#[2,] -0.5171526
#[3,] -0.4883374
#[4,]  0.2651266
#> var
#[1] 0.01419242
#> cons
#[1] 0.079781