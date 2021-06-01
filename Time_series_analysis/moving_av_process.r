# Specify parameters for MA(q) process
n = 10000
# first weight should be 1
weights= c(1,0.4,0.2,0.6,0.5)

# Generate noise
set.seed(42)
noise = rnorm(n)

#for q=2, n=10000
#ma_2 = NULL
#for (i in 3:10000){
#  ma_2[i]=noise[i]+0.7*noise[i-1]+0.2*noise[i-2]
#}
# moving_average_process=ts(ma_2[3:10000])

ma_q = NULL
q = length(weights)-1
for (i in 1:q){
  ma_q[i] = as.numeric(t(matrix(weights[1:i]))%*%(matrix(noise[i:1])))
}

#start (i+q) from q+1, ... , n
#X[q+1] = w[1]*Z[q+1] + ... + w[q+1]*z[1]
#X[n] = w[1]*Z[n] + ... + w[q+1]*z[n-q]
for (i in 1:(n-q)){
  ma_q[(i+q)]=as.numeric(t(matrix(weights))%*%(matrix(noise[(i+q):i])))
}

# Alternatively, time can be shifted
#for (i in q:n){
#  ma_q[(i-(q-1))]=as.numeric(t(matrix(weights))%*%(matrix(noise[i:i-q])))
#}

moving_average_process=ts(ma_q[1:n])

#2rows, 1col
par(mfrow=c(2,1)) 
plot(moving_average_process)
acf(moving_average_process)