# Season statistics for individual golfers on the United States LPGA and PGA tours.
# The first column reports each player's average driving distance in yards. 
# The second column reports the percentage of the player's drives that finish in the fairway, measuring their accuracy. 
# The third and final column has a 1 to denote a female golfer (on the LPGA tour), and a 2 to denote male golfer (on the PGA tour).

dat=read.table("http://users.stat.ufl.edu/~winner/data/pgalpga2008.dat",header=F)
attach(dat)
names(dat) <- c("dr_dist", "accuracy", "FM")
dat$FM[dat$FM==1] <- 0 # Female
dat$FM[dat$FM==2] <- 1 # Male

# Model is b0 + b1x1 + b2x2, x1 = driv_dist, x2 = FM
dat.lm=lm(accuracy~dr_dist+FM, data=dat)
summary(dat.lm)

# b0 has no meaningful physical interpretation 
# because it represents the percentage accuracy of a female golfer (x2=0)
# who drives the ball 0 yards on average (x1=0)

# Interpretation of b1: holding all else constant, 
# each additional yard of distance is associated with a 0.323 decrease in drive accuracy percentage
# The standard error for b1 (which we can think of as marginal posterior standard deviation in this case) 
# is roughly 1/10 times the magnitude of the posterior mean estimate b1_hat (Prob(b1<0) is large)
# i.e., the posterior mean b1_hat is more than 10 posterior standard deviations from 0

# The estimated value of b_2 would typically be interpreted to mean that holding all else constant 
# (for a fixed driving distance)
# golfers on the PGA tour are about 9% more accurate with their drives on average than golfers on the LPGA tour
# PGA tour golfers' average drives are 40+ yards longer than LPGA tour golfers' average drives
# LPGA tour golfers are actually more accurate on average
# b2 is actually a correction for the discrepancy in driving distances

# Patterns in the plot of yhat versus residuals, for example, can indicate an inadequacy in the model
# Residuals appear to be random and lack any patterns or trends, however, there is at least one outlier
plot(fitted(dat.lm), residuals(dat.lm))