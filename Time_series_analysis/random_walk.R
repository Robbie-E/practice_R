# Random walk: x(t) = x(t-1) + Z(t)
n = 10000
weight = 1

set.seed(42)
noise = rnorm(1)

x=NULL
x[1]=0
for(i in 2:n){x[i]=weight*x[i-1]+noise}
random_walk = ts(x)

#2rows, 1col
par(mfrow=c(2,1))

plot(random_walk)
acf(random_walk)

# remove the trend
# diff = Z(t) ~ norm
#2rows, 1col
par(mfrow=c(2,1))
plot(diff(random_walk))
acf(diff(random_walk))