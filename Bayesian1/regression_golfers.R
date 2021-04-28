# Season statistics for individual golfers on the United States LPGA and PGA tours.
# The first column reports each player's average driving distance in yards. 
# The second column reports the percentage of the player's drives that finish in the fairway, measuring their accuracy. 
# The third and final column has a 1 to denote a female golfer (on the LPGA tour), and a 2 to denote male golfer (on the PGA tour).

dat=read.table("http://users.stat.ufl.edu/~winner/data/pgalpga2008.dat",header=F)
attach(dat)
names(dat) <- c("dr_dist", "accuracy", "FM")
#"FM" is the name of the third column (replace "FM" with whatever you named this column)
# select=1:2 means to include columns 1 and 2 in the new data set "datF".
datF <- subset(dat, FM==1, select=1:2)
datM <- subset(dat, FM==2, select=1:2)

# posterior mean estimate of the slope b is about five standard errors below 0 
# i.e., posterior probability that this slope is negative is near 1
# For each additional yard of driving distance, we expect to see a decrease in percentage accuracy of |b|
datF.lm=lm(accuracy~dr_dist, data=datF)
datM.lm=lm(accuracy~dr_dist, data=datM)
summary(datF.lm)

# Drive distance and accuracy are negatively correlated
# greater distances are associated with less accuracy.
par(mfrow=c(1,2))
plot(datF$dr_dist, datF$accuracy)
lines(datF$dr_dist,fitted(datF.lm))
plot(datM$dr_dist, datM$accuracy)
lines(datM$dr_dist,fitted(datM.lm))

# posterior predictive mean estimate of accuracy for a new female golfer with average driving distance of 260 yd
driv_star = 260
predF = coef(datF.lm)[1] + coef(datF.lm)[2]*driv_star 

# Predictive intervals provide probability statements about a new observation.
# 95% posterior predictive interval for the accuracy of a new female golfer with average driving distance of 260 yd
# For a new female golfer who averages 260 yards per drive, probability that her accuracy will be in the interval is .95.
# posterior prediction interval (same as frequentist)
# can be obtained from quantile of t-distribution with df=n-2
n_F = dim(datF)[1] #157
df_F=n_F-2
SE_resF=5.246 #from summary of datF.lm, estimate of sigma
scaleF=SE_resF*sqrt( 1+(1/n_F)+(((driv_star-mean(datF$dr_dist))^2)/((n_F-1)*var(datF$dr_dist))) )
lwr=predF-qt(.975,df_F)*scaleF
upr=predF+qt(.975,df_F)*scaleF
# can be done automatically with R
predict(datF.lm, data.frame(dr_dist=driv_star),interval="predict")

#n_M = dim(datM)[1] #197