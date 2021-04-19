#SARIMA modeling on J&J data using astsa package
library(astsa)
library(forecast)

# look at time plot
# de-trending transformation: log-return
par(mfrow=c(3,1))
plot(diff(log(jj)))
acf(diff(log(jj)))
acf(diff(log(jj)), type='partial') 

# differencing: D=1
# we saw a seasonal differencing s=4 since this is quarterly data
data = diff(diff(log(jj)), 4)

# Ljung-Box test for data
# Test H0: several acf coefficients are zero
# if p-value (0.00029)  < alpha, reject H0
# Box.test(data, lag=log(length(data)), type="Ljung-Box")

par(mfrow=c(3,1))
plot(data, ylab = 'diff(diff(log(jj)), 4)')
acf(data)
acf(data, type='partial') 
# ACF -> q=0,1; Q=0,1
# PACF -> p=0,1; P=0,1

# look for SARIMA(p,1,q,P,1,Q)[4] for log(jj) for 0<=p,q,P,Q <=1
# Notation: arima(x=log(jj), order=c(p,1,q), seasonal=list(order=c(P,1,Q), period=4))
d=1; DD=1; per=4;
for(p in 1:2){
  for(q in 1:2){
    for(P in 1:2){
      for(Q in 1:2){
        if(p+d+q+P+DD+Q<=10){
          model<-arima(x=log(jj), order = c((p-1),d,(q-1)), seasonal = list(order=c((P-1),DD,(Q-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,P-1,DD,Q-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}

# Model log(jj) as SARIMA(0,1,1,1,1,0)[4] since it has lowest AIC
# Show Ljung-Box test for residuals (H0 not rejected indicates whiteness)
sarima(log(jj), 0,1,1,1,1,0,4)

# Forecast future values using forecast package
model<-arima(x=log(jj), order=c(0,1,1), seasonal=list(order=c(1,1,0), period=4))
par(mfrow=c(2,1))
plot(log(jj))
plot(forecast(model), ylab = 'log(jj)')
# forecast(model) tells the point estimates and confidence intervals