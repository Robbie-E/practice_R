#simulate MA(2) process, beta_1 = 0.7, beta_2 = 0.2
set.seed(42)
n = 10000
# Generate noise
noise = rnorm(n)
ma_2 = NULL
#start from k+1  
for (i in 3:10000){
  ma_2[i]=noise[i]+0.7*noise[i-1]+0.2*noise[i-2]
}
moving_average_process=ts(ma_2[3:10000])
#2rows, 1col
par(mfrow=c(2,1)) 
plot(moving_average_process)
acf(moving_average_process)

#simulate MA(3) process given beta_1, beta_2, beta_3 (beta_0 = 1)
k=3
n = 10000
# Generate noise
noise = rnorm(n)
ma_3 = NULL
weights= c(0.6,0.2,0.4,1)
#start from k+1  
for (i in 1:(n-k)){
  ma_3[(i+k)]=as.numeric(t(matrix(weights))%*%(matrix(noise[i:(i+k)])))
}
moving_average_process=ts(ma_3[(k+1):n])
#2rows, 1col
par(mfrow=c(2,1)) 
plot(moving_average_process)
acf(moving_average_process)

#Simulate MA(2)
par(mfrow=c(2,1))
data.ts=arima.sim(n=1000, model=list(ma=c(0.5,0.5)))
plot(data.ts)
acf(data.ts)