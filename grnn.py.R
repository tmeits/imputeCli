#IVA 1.1.17 / 4.5.17#
#rm(list=ls())

require(compiler)
enableJIT(3)

#
grnn.activator <- function(data, train_x, sigma) {
  #stopifnot(is.vector(data), is.vector(train_x)) # Check slows the calculation in 3 times
  distance <- 0.
  for (i in 1:length(data)){
    distance <- distance + (data[i] - train_x[i]) ^ 2
  }
  return(exp( - distance / (sigma ^ 2)))
}

#
grnn.kernel <- function(data, train_x, train_y, sigma) {
  out_dim <- ncol(train_y)
  result <- 1:out_dim
  nrow_train_x <- 1:nrow(train_x)
  for (dim in 1:out_dim) {
    factor <- 0.; divide <- 0;
    for (i in nrow_train_x) {
      cache <- grnn.activator(data, train_x[i, ], sigma)
      factor <- factor + train_y[i, dim] * cache
      divide <- divide + cache
    }
    result[dim] <- (factor / divide)
  }
  return(result)
}

#  
grnn.sim <- function(Ga, Xa, Ya, sigma) {
  len <- nrow(Ga)
  res <- as.matrix(1:len)
  for(i in 1:len) {
    res[i] <- grnn.kernel(Ga[i, ], Xa, Ya, sigma)
  }
  return(res)
}

#
grnn.impute <- function() {
  
}

require(grnn) #https://github.com/cran/grnn/blob/master/R/guess.r
grnn.fit <- function(Ga, Xa, Ya, sigma) {
  len <- nrow(Ga)
  res <- as.matrix(1:len)
  grnn.net <- smooth(learn(data.frame(Ya,Xa)), sigma)
  for(i in 1:len) {
    res[i] <- guess(grnn.net, Ga[i, ])
  }
  return(res)
}
#
grrn.pso <-function(Xa, Ya) {
  #Initialization of PSO algorithm
  #Calculate the initial fitness value of each particle
  
}

if (FALSE) {
  train_x <-   matrix(c(3,5,  3,11,  8,6,  0,34,  13,3,  2,17,  23,2,  37,1,  1,40,  21,30,  30,24,  24,64,  43,46,  31,51), ncol=2, byrow = TRUE)
  train_y <-   as.matrix(c(-10,  -10,  -10,  -10,  -10,  -10,  -10,  -10,  -10,  0,  0,  10,  10,  10))
  
  y.sim <- grnn.sim(train_x, train_x, train_y/20, 15)
  plot(train_y, col="blue")
  points(y.sim*20, col="red")
}

if (FALSE) {
  #https://github.com/cran/grnnR/blob/master/R/grnn.r
  n <- 365*7; set.seed(123456); sigma <- 0.1
  x <- as.matrix(runif(n, -2, 2))
  y <- as.matrix(x^3 + rnorm(n, 0, .1))
  plot(x,y, col="blue")
  x.sample <- x
  y.sim <- grnn.sim(x, x.sample, y, sigma)
  points(x.sample, y.sim, col="red")
  y.sim <- grnn.fit(x, x.sample, y, sigma)
  points(x.sample, y.sim, col="green")
  
  system.time(grnn.sim(x, x.sample, y, sigma)) #35.51 / 51.69 TURION ZM82
  system.time(grnn.fit(x, x.sample, y, sigma)) #33.42 /  41.06
  
}
if (FALSE) {

}



