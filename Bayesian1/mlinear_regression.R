# Multiple regression
# http://www.randomservices.org/random/data/Galton.txt
# Galton's seminal data on predicting the height of children from the 
# heights of the parents, all in inches

heights=read.table("http://www.randomservices.org/random/data/Galton.txt",header=T)
attach(heights)
names(heights)

#pairs(heights)

summary(lm(Height~Father+Mother+Gender+Kids))
# Kids have very small slope, estimate ~ standard error
# including too many variables will increase your predictive variance

summary(lm(Height~Father+Mother+Gender))
heights.lm=lm(Height~Father+Mother+Gender)
# how to interpret intercept?
# each extra inch taller a father is is correlated with 0.4 inch extra height in the child
# each extra inch taller a mother is is correlated with 0.3 inch extra height in the child
# a male child is on average 5.2 inches taller than a female child (1=M, 0=F)

# 95% posterior probability interval for the the difference in height between boys and girls
# Marginal distribution for the slope parameter of gender is a t-distribution with df=894
GenderM_slope = 5.226
SE_genderM=0.144
df=894
GenderM_slope - qt(.975,df)*SE_genderM # or GenderM_slope + qt(.025,df)*SE_genderM
GenderM_slope + qt(.975,df)*SE_genderM

# posterior prediction interval (reference Bayesian analysis give the same interval as frequentist)
# What is the predicted height and predicted 95% probability interval for a child's height 
# when father is 68 in tall and mother is 64 in tall for either male or female child
predict(heights.lm,data.frame(Father=68,Mother=64,Gender="M"),interval="predict")
predict(heights.lm,data.frame(Father=68,Mother=64,Gender="F"),interval="predict")