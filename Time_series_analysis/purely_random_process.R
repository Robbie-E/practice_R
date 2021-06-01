#covariance - measures linear dependence of 2 random vars
#acf(time_series, type='covariance')
#cov()

set.seed(42)
purely_rand_process=ts(rnorm(100))

par(mfrow=c(3,1))

plot(purely_rand_process)
acf(purely_rand_process, type='covariance') 
#gives plot of autocovariances, default type is autocorrelation function
acf(purely_rand_process) 