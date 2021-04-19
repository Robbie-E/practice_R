#Simulate AR(1)
set.seed(42); N=1000; beta=.4;
z=rnorm(N,0,1); X=NULL;
X[1] = z[1];
for (t in 2:N){X[t] = z[t]+beta*X[t-1]}
X.ts = ts(X)
par(mfrow=c(3,1))
plot(X.ts)
acf(X.ts)
acf(X.ts, type="partial")

#Simulate AR(2)
par(mfrow=c(3,1))
data.ts=arima.sim(n=1000, list(ar=c(0.7,0.2)))
plot(data.ts)
acf(data.ts)
acf(data.ts, type="partial")

#Simulate AR(2)
par(mfrow=c(3,1))
data.ts=arima.sim(n=1000, list(ar=c(0.5,-0.4)))
plot(data.ts)
acf(data.ts)
acf(data.ts, type="partial")

#Simulate AR(2)
par(mfrow=c(3,1))
data.ts=arima.sim(n=1000, list(ar=c(0.6,0.2)))
plot(data.ts)
acf(data.ts)
acf(data.ts, type="partial")

#Simulate AR(3)
par(mfrow=c(3,1))
data.ts=arima.sim(n=1000, list(ar=c(0.4,0.2,0.3)))
plot(data.ts)
acf(data.ts)
acf(data.ts, type="partial")