#Use AIC to assess model quality
rm(list=ls(all=TRUE))
set.seed(42)

data = arima.sim( list(order = c(2,0,0), ar =c( 0.7, -.2)), n = 2000)
par(mfrow=c(2,1))
acf(data, main="ACF of AR Data of Second Order")
acf(data, type="partial", main="PACF of Time Series")

print(arima(data, order=c(2,0,0), include.mean=FALSE ))

# model selection
SSE=NULL
AIC=NULL
for (p in 1:5) {
  m = arima(data, order=c(p,0,0), include.mean=FALSE )
  SSE[p] = sum(resid(m)^2)
  AIC[p] = m$aic
  print( m$coef )
  print( paste(m$aic, sum(resid(m)^2)) )
}

par(mfrow=c(2,1))
order=c(1,2,3,4,5)
plot(SSE~order, main="SSE plotted on Order of AR(p) Model", ylim=c(1800, 2100))
plot(AIC~order, main="AIC plotted on Order of AR(p) Model", ylim=c(5500, 5800))

#data = arima.sim( list(order = c(3,0,0), ar =c( 0.6, -0.1, .4)), n = 5000)
#m2 = arima(data, order=c(2,0,0), include.mean=FALSE)
#m3 = arima(data, order=c(3,0,0), include.mean=FALSE)
#m4 = arima(data, order=c(4,0,0), include.mean=FALSE)
#SSE[p] = sum(resid(m1)^2)
