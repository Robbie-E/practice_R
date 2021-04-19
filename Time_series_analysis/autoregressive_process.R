#Simulate AR(p) process
N = 10000
#sum of weights should be <=1
weights = c(0.1,0.2,0.3,0.3,0.1)

set.seed(42)
noise = rnorm(N,0,1)

#for p=2
#X[1] = Z[1];
#for (t in 2:N){
#  X[t] = Z[t]+beta*X[t-1]
#}

ar_p = NULL
p = length(weights)
ar_p[1] = noise[1]
for (i in 2:p){
  ar_p[i] = noise[i] + as.numeric(t(matrix(weights[1:(i-1)]))%*%(matrix(ar_p[(i-1):1])))
}

#start (i+p) from p+1, ... , n
#X[p+1] = z[p+1] + w[1]*X[p] + ... + w[p]*X[1]
#X[n] = z[n] + w[1]*X[n-1] + ... + w[p]*X[n-p]
for (i in 1:(N-p)){
  ar_p[(i+p)]= noise[(i+p)] + as.numeric(t(matrix(weights))%*%(matrix(ar_p[(i+p-1):i])))
}
ar_p.ts = ts(ar_p)

# Plot correlation functions
par(mfrow=c(3,1))
plot(ar_p.ts)
acf(ar_p.ts)
pacf(ar_p.ts)
