yule_walker <- function(dframe, p=1){
  #setwd("~/R Scripts")
  #dframe <- ts(read.csv(data))

  # subtract mean to get a time series with mean zero
  ar.process = dframe - mean(dframe)
  
  r=NULL
  # disregard the first term since it is always 1
  r[1:p] = acf(ar.process, plot=F)$acf[2:(p+1)]
  c0 = acf(ar.process, type='covariance', plot=F)$acf[1]
  
  # Form R matrix from ACF
  R = matrix(1,p,p) 
  for (i in 1:p){
    for (j in 1:p){
      if(i!=j){
        R[i,j] = r[abs(i-j)]
      }
    }
  }
  
  # b-column vector on the right
  b=matrix(r,p,1)
  
  # Solve Rx=b and find phi's
  phi.hat = solve(R,b)
  
  # Estimate variance of white noise
  var.hat=c0*(1-sum(phi.hat*r))
  
  # Constant term in the AR(p) model
  phi0.hat=mean(dframe)*(1-sum(phi.hat))
  
  result = list(phi = phi.hat, var = var.hat, cons = phi0.hat)
}