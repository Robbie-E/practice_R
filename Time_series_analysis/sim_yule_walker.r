#Solve yule walker for any AR(p) process given ACF, c0
acf.given=c(1,0.8,0.6,0.2)
c0=5
p=length(acf.given)-1

#form R matrix
R=matrix(1,p,p) 
for (i in 1:p){
  for (j in 1:p){
    if(i!=j){
      R[i,j] <- acf.given[abs(i-j)]
    }
  }
}

# b-column vector on the right
b=matrix(acf.given[2:(p+1)],p,1)

# solve Rx=b and find phi's
phi.hat=solve(R,b)
var.hat=c0*(1-sum(phi.hat*acf.given[2:(p+1)]))