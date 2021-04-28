# single variable linear regression
# http://www.randomservices.org/random/data/Challenger2.txt
# 23 previous space shuttle launches before the Challenger disaster
# T is the temperature in Fahrenheit, I is the O-ring damage index

oring=read.table("http://www.randomservices.org/random/data/Challenger2.txt",header=T)
attach(oring)
#note: masking T=TRUE

plot(T,I)
# y = damage index, X = temperature
oring.lm=lm(I~T)
summary(oring.lm)
# Std. Error are standard devs for marginal distributions for the intercept and slope
# t values and the p-values associated with the t-tests are used for the frequentist significance test

# add fitted line to scatterplot
lines(T,fitted(oring.lm))

# 95% posterior probability interval for the slope
SE_temp = 0.06349
est_slope = -0.24337 # Temp
# roughly for every 4 degrees the temp gets colder, we see a 1 unit increase in damage index
# marginal posterior distribution is a t-distribution with 21 degree of freedom 
# and scaling factor of SE_temp
SE_res=2.102
df=21
n=23

est_slope - qt(.975,df)*SE_temp
est_slope + qt(.975,df)*SE_temp
# the interval does not include zero, there is negative relationship between temperature and damage
# note that these are the same as the frequentist confidence intervals 
# when using standard reference prior for Bayesian analysis

# the Challenger launch was at 31 degrees Fahrenheit
# how much o-ring damage would we predict? i.e. y-hat
Tstar=31
coef(oring.lm)
pred = coef(oring.lm)[1] + coef(oring.lm)[2]*Tstar  

# posterior prediction interval (same as frequentist) using Bayesian reference prior
# If we launch at Tstar=31, what is a 95% predictive interval for what we might see in damage?
predict(oring.lm,data.frame(T=31),interval="predict") 
# prediction interval calculated from scratch
scale=SE_res*sqrt( 1+(1/n)+(((Tstar-mean(T))^2)/((n-1)*var(T))) )
pred-qt(.975,df)*scale #lower bound damage of 95% predictive interval
pred+qt(.975,df)*scale #upper bound damage of 95% predictive interval

# Bayesian: posterior probability that damage index is greater than zero if we launch at 31 degrees
# we use the same predictive distribution, t-distribution with this center (mean=pred) and scale
center = 0-pred
1-pt(center/scale,df) #pt gives probability less than zero