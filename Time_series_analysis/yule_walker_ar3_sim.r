# x_t=phi1*x_(t-1)+phi2* x_(t-2)+\phi_3*x_(t-3)+z_t
# z_t~ N(0, sigma^2)

set.seed(2017)
#model parameters (that will be estimated)
sigma=4
phi=NULL
phi[1:3]=c(1/3,1/2,7/100)
n=100000

#simulate ar3 process
ar3.process=arima.sim(n,model=list(ar=phi), sd=sigma)

#find autocorrelations
r=NULL
r[1:3]=acf(ar3.process, plot=F)$acf[2:4]

#form R matrix
R=matrix(1,3,3) 
R[1,2]=r[1] 
R[1,3]=r[2]
R[2,1]=r[1]
R[2,3]=r[1]
R[3,1]=r[2]
R[3,2]=r[1]

# b-column vector on the right
# b- column vector with no entries
b=matrix(,3,1)
b[1,1]=r[1]
b[2,1]=r[2]
b[3,1]=r[3]

# solve Rx=b and find phi's
phi.hat=solve(R,b)

# sigma estimation
c0=acf(ar3.process, type='covariance', plot=F)$acf[1]
var.hat=c0*(1-sum(phi.hat*r))

#Plots
par(mfrow=c(3,1))
plot(ar3.process, main='Simulated AR(3)')
acf(ar3.process, main='ACF')
pacf(ar3.process, main='PACF')