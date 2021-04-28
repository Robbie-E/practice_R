#Normal conjugate model with mu and sigma unknown

# Specify sample and prior parameters
#n, no. of data points
#xbar, sample mean
#s2, sample variance
#m, prior mean for mu|sigma2 (normal prior)
#w, effective sample size for prior for mu|sigma2 (normal prior)
#a, b priors for sigma2 (inverse gamma prior)

rinvgamma <- function(n, shape, rate){
  # Inverse gamma function for shape=a, rate=b
  z <- rgamma(n, shape, rate)
  x <- 1/z 
}

est_mu <- function(n, xbar, s2, m, w, a, b){
  # Inverse gamma function for posterior dist. of sigma2|x
  a_post = a+(0.5*n)
  b_post = b+(0.5*(n-1)*s2)+((0.5*n*w/(n+w))*(xbar-m)^2)
  sigma2 <- rinvgamma(n=1000, shape=a_post, rate=b_post)
  
  # Normal distribution for posterior dist of mu|sigma2,x
  m_post = ((n*xbar)+(w*m))/(n+w)
  mu <- rnorm(1000, mean=m_post, sd=sqrt(sigma2/(w+n)))
  cred_int = quantile(x=mu, probs=c(0.025, 0.975))
  list(mu=mu, cred_int=cred_int)
}

#mean(est_mu(27,609.7,401.8,500,0.1,3,200)$mu)
#est_mu(27,609.7,401.8,500,0.1,3,200)$cred_int
#mean(est_mu(30,622.8,403.1,500,0.1,3,200)$mu)
#est_mu(30,622.8,403.1,500,0.1,3,200)$cred_int

muA <- est_mu(30,622.8,403.1,500,0.1,3,200)$mu
muB <- est_mu(27,609.7,401.8,500,0.1,3,200)$mu
#mean(muA > muB)