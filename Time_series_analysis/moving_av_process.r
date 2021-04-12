#specify parameters
n = 10000
weights= c(1,0.4,0.2,0.6,0.5)

# Generate noise
noise = rnorm(n)

ma_k = NULL
k = length(weights)-1
  
#for k=2, n=10000
#ma_2 = NULL
#for (i in 3:10000){
#  ma_2[i]=noise[i]+0.7*noise[i-1]+0.2*noise[i-2]
#}
# moving_average_process=ts(ma_2[3:10000])


#start (i+k) from k+1, ... , n
#X[k+1] = w[1]*Z[K+1] + ... + w[k+1]*z[1]
#X[n] = w[1]*Z[n] + ... + w[k+1]*z[n-k]
for (i in 1:(n-k)){
  ma_k[(i+k)]=as.numeric(t(matrix(weights))%*%(matrix(noise[(i+k):i])))
}

#for (i in k:n){
#  ma_k[(i-(k-1))]=as.numeric(t(matrix(weights))%*%(matrix(noise[i:i-(k-1)])))
#}

moving_average_process=ts(ma_2[(k+1):n])

#2rows, 1col
par(mfrow=c(2,1)) 

plot(moving_average_process)
acf(moving_average_process)