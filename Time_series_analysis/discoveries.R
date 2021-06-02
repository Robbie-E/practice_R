#Using discoveries dataset
par(mfcol = c(2,1))
plot(discoveries, main = "Time Series of Number of Major Scientific Discoveries in a Year")
stripchart(discoveries, method='stack', offset=.5, at=.15, pch=19, main="Number of Discoveries Dotplot",
           xlab="Number of Major Scientific Discoveries in a Year",
           ylab="Frequency")
par(mfcol = c(2,1))
acf(discoveries, main="ACF of Number of Major Scientific Discoveries in a Year")
acf(discoveries, type="partial", main="PACF of Number of Major Scientific Discoveries in a Year")

#model selection, no differencing
#AIC( arima( discoveries, order=c(0,0,1) ) ) #AIC = [1] 445.5895
#AIC( arima( discoveries, order=c(0,0,2) ) ) #AIC = [1] 444.6742
#AIC( arima( discoveries, order=c(0,0,3) ) ) #AIC = [1] 441.323
#AIC( arima( discoveries, order=c(1,0,0) ) ) #AIC = [1] 443.3792
#AIC( arima( discoveries, order=c(1,0,1) ) ) #AIC = [1] 440.198
#AIC( arima( discoveries, order=c(1,0,2) ) ) #AIC = [1] 442.0428
#AIC( arima( discoveries, order=c(1,0,3) ) ) #AIC = [1] 442.6747
#AIC( arima( discoveries, order=c(2,0,0) ) ) #AIC = [1] 441.6155
#AIC( arima( discoveries, order=c(2,0,1) ) ) #AIC = [1] 442.0722
#AIC( arima( discoveries, order=c(2,0,2) ) ) #AIC = [1] 443.7021
#AIC( arima( discoveries, order=c(2,0,3) ) ) #AIC = [1] 441.6594
#AIC( arima( discoveries, order=c(3,0,0) ) ) #AIC = [1] 441.5658
#AIC( arima( discoveries, order=c(3,0,1) ) ) #AIC = [1] 443.5655
#AIC( arima( discoveries, order=c(3,0,2) ) ) #AIC = [1] 439.9263
#AIC( arima( discoveries, order=c(3,0,3) ) ) #AIC = [1] 441.2941

library(forecast)
print(auto.arima(discoveries, d=0, approximation=FALSE))
print(auto.arima(discoveries, d=0, approximation=TRUE))
# metric: ic=c("aicc", "aic", "bic")
print(auto.arima(discoveries, d=0, ic="bic", approximation=FALSE))