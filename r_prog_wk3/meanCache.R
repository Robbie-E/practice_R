makeVector <- function(x = numeric()) {
  # a list of four functions to set/get input and set/get mean cache
  m <- NULL
  set <- function(y) { #can be used to modify input vector, cache is emptied
    x <<- y  #assign input y to x in the parent env
    m <<- NULL  #clear/reset cached mean m in parent
  }
  get <- function() x  #get vector x from parent
  #assign value of mean to m in parent, store mean in cache
  setmean <- function(mean) m <<- mean  
  getmean <- function() m  #get mean m from parent
  list(set = set, get = get, setmean = setmean, getmean = getmean)
}

cachemean <- function(x, ...) {
  #get mean from cache and skip calculation if m is not NULL (i.e. cache is not empty)
  #calculation of mean is skipped for future repeats of cachemean()
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m) 
  }
  #calculate mean from input for first instance calculation if cache is empty
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)  #assign calculated mean to m in parent
  m
}

# test output
#> vec <- c(1,2,3)
#> avec <- makeVector(vec)
#> avec$get()          # get the input vector
#[1] 1 2 3
#> avec$getmean()      # get mean before subjecting to cachemean()
#NULL
#> cachemean(avec)     # first instance mean calculation
#[1] 2
#> avec$getmean()      # look at cached mean
#[1] 2
#> cachemean(avec)     # second instance mean calculation 
#getting cached data
#[1] 2

#> avec$setmean(100)   # tamper the mean cache
#> avec$getmean()      # get the tampered mean
#[1] 100
#> cachemean(avec)     # cachemean gets the value in cache
#getting cached data
#[1] 100

#> avec$set(c(3,4,5))  # modify input vector
#> avec$get()          # get the new input vector
#[1] 3 4 5
#> avec$getmean()      # modifying input resets previous mean to NULL
#NULL
#> cachemean(avec)     # calculate mean for new input vector
#[1] 4
#> cachemean(avec)
#getting cached data
#[1] 4