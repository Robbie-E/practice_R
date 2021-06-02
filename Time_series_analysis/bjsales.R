#Simulating ARIMA on BJsales dataset
# Plot time series 'BJsales'
plot(BJsales)
plot(diff(BJsales))
plot(diff(diff(BJsales)))
pacf(diff(diff(BJsales)))
acf(diff(diff(BJsales)))
#try different models and compare AIC
d=2
for(p in 1:4){
  for(q in 1:2){
    if(p+d+q<=6){
      model<-arima(x=BJsales, order = c((p-1),d,(q-1)))
      pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
      sse<-sum(model$residuals^2)
      cat(p-1,d,q-1, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
    }
  }
}
#model to ARIMA(0,2,1), check the whiteness of residuals
model<-arima(BJsales, order=c(0,2,1))
par(mfrow=c(2,2))
plot(model$residuals)
acf(model$residuals)
pacf(model$residuals)
qqnorm(model$residuals)
model

# 0 2 0 AIC= 577.6777  SSE= 423.7908  p-VALUE= 7.610494e-07 
# 0 2 1 AIC= 517.1371  SSE= 276.2293  p-VALUE= 0.9632467 
# 1 2 0 AIC= 541.9646  SSE= 327.92    p-VALUE= 0.003606979 
# 1 2 1 AIC= 518.9734  SSE= 275.8554  p-VALUE= 0.941776 
# 2 2 0 AIC= 532.2986  SSE= 302.7467  p-VALUE= 0.05824473 