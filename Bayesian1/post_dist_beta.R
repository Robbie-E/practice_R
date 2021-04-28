#We are giving 2 students a 4-choice examof 40 items. We think they weill do better than guessing randomly.
#1) what are the parameters of interest?
#2) what is our likelihood?
#3) what prior to use
#4) what is the prior probability P(theta>0.25)? >0.5? >0.8?
#5) Suppose the first student gets 33 right answers. What is the posterior distribution for theta1?
  #P(theta1>.25)? >0.5? >0.8? What is a 95% credible interval for theta1?
#6) Supose the second student gets 24 questions right. What is the posterior distribution for theta2? 
  #P(theta2>.25)? >0.5? >0.8? What is a 95% credible interval for theta1?
#7) What is the posterior probability that theta1 > theta2? i.e. first student has a higher chance of getting a question right

#Parameters: theta1, theta2 probability that student 1/2 gets a question right
#Likelihood: Binomial(40, theta) 
#assuming that each questions are independent and theta is constant for all questions

#The conjugate prior is beta prior.
theta=seq(from=0, to=1, by=0.01)
#uniform prior: 
plot(theta, dbeta(theta,1,1), type="l")
#we assume them to be correct about 2 times in 3 (a/a+b = 2/3): 
plot(theta, dbeta(theta,4,2), type="l") #there is still some mass below 0.25
plot(theta, dbeta(theta,8,4), type="l") #increase n_eff, we use this

#Find prior probabilities: 
theta_thresh = c(0.25,0.5,0.8)
sapply(theta_thresh, function(x){1-pbeta(x, 8, 4)}) #0.998, 0.886, 0.161

#Posterior distribution for theta1: Y1=33, n=40 (MLE: 33/40 = 0.825)
#Beta(8+33, 4+40-33) with posterior mean: 41/(41+11) = 0.788
plot(theta, dbeta(theta,41,11), type="l")
lines(theta, dbeta(theta,8,4), lty=2)
lines(theta, 44*dbinom(34, size=40, p=theta), lty=3)

#Find posterior probabilities for theta1:
sapply(theta_thresh, function(x){1-pbeta(x, 41, 11)}) #1, 0.99, 0.44
#Find equal-tailed 95% credible interval:
qbeta(0.025, 41,11)
qbeta(0.975, 41,11)
#theta1 between 0.67 and 0.89
  
  
#Posterior distribution for theta2: Y2=24, n=40 (MLE: 24/40 = 0.6)
#Beta(8+24, 4+40-24) with posterior mean: 32/(32+20) = 0.615
plot(theta, dbeta(theta,32,20), type="l")
lines(theta, dbeta(theta,8,4), lty=2)
lines(theta, 44*dbinom(24, size=40, p=theta), lty=3)

#Find posterior probabilities for theta2:
sapply(theta_thresh, function(x){1-pbeta(x, 32, 20)})   # 1, 0.95, 0.0012
#Find equal-tailed 95% credible interval:
qbeta(0.025, 32,20)
qbeta(0.975, 32,20)
#theta1 between 0.48 and 0.74

#Probability that theta1>theta2:
#Simulate by drawing 10000 samples from both posterior distributions of theta1 and theta2.
samples = 10000
theta1=rbeta(samples,41,11)
theta2=rbeta(samples,32,40)
mean(theta1>theta2)