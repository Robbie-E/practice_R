post.pol <- function(pref, yeses, cprior=0.5) {
  #pref is guess of political preference, i.e. con or lib
  #yeses is no.of yes in the questionnaire
  #cprior is unconditional probability of being con
  #output is P(preference|yeses)
  normf <- (dbinom(yeses,size=5,prob=0.8)*cprior)+(dbinom(yeses,size=5,prob=0.3)*(1-cprior))
  if(identical(pref,'con')){
    post <- (dbinom(yeses,size=5,prob=0.8)*cprior)/normf
  }
  if(identical(pref,'lib')){
    post <- (dbinom(yeses,size=5,prob=0.3)*(1-cprior))/normf
  }  
  post
}
