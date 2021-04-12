#xt=phi1*x(t-1)+phi2*x_(t-2)+z_t 
#z_t~ N(0, sigma^2)

set.seed(2017)
# model parameter (to be estimated)
sigma=4
phi=NULL
phi[1:2]=c(1/3,1/2)
n=10000

#simulate ar2 process
ar.process=arima.sim(n,model=list(ar=phi), sd=sigma)

r=NULL
r[1:2]=acf(ar.process, plot=F)$acf[2:3]

#form R matrix
R=matrix(1,2,2) # matrix of dimension 2 by 2, with entries all 1's.
R[1,2]=r[1] # only off-diagonal entries are edited
R[2,1]=r[1]

#b vector on the right
b=matrix(r,nrow=2,ncol=1)

# solve Rx=b and find phi's
phi.hat=solve(R,b)

#estimate variance
c0=acf(ar.process, type='covariance', plot=F)$acf[1]
var.hat=c0*(1-sum(phi.hat*r))

#Plots
par(mfrow=c(3,1))
plot(ar.process, main='Simulated AR(2)')
acf(ar.process, main='ACF')
pacf(ar.process, main='PACF')