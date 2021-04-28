post.coin3 <- function(guess, heads, prior1, prior2, nflips=4) {
  #guess is an outcome (fair, head-loaded, tail-loaded)
  #heads is number of heads after nflips flips of coin
  #prior1 is unconditional probability that coin is fair
  #prior2 is unconditional probability that coin is hload
  prob1 <- 0.5
  prob2 <- 0.7 
  prob3 <- 0.3
  normf <- (dbinom(heads,size=nflips,prob=prob1)*prior1)+(dbinom(heads,size=nflips,prob=prob2)*prior2)+(dbinom(heads,size=nflips,prob=prob3)*(1-prior1-prior2))
  if(identical(guess,'fair')){
    post <- (dbinom(heads,size=nflips,prob=prob1)*prior1)/normf
  }
  if(identical(guess,'hload')){
    post <- (dbinom(heads,size=nflips,prob=prob2)*prior2)/normf
  }
  if(identical(guess,'tload')){
    post <- (dbinom(heads,size=nflips,prob=prob3)*(1-prior1-prior2))/normf
  }  
  post
}