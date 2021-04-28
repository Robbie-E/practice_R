post.coin2 <- function(guess, heads, prior, nflips=1) {
  #guess is an outcome (loaded or fair)
  #heads is number of heads after nflips flips of coin
  #prior is unconditional probability that coin is loaded
  prob1 <- 0.7
  prob2 <- 0.5 
  normf <- (dbinom(heads,size=nflips,prob=prob1)*prior)+(dbinom(heads,size=nflips,prob=prob2)*(1-prior))
  if(identical(guess,'load')){
    post <- (dbinom(heads,size=nflips,prob=prob1)*prior)/normf
  }
  if(identical(guess,'fair')){
    post <- (dbinom(heads,size=nflips,prob=prob2)*(1-prior))/normf
  }
  post
}