#sample some obersvations from a logit model without any optimization techniques

rlogit <- function(n, beta){
  X <- matrix(rnorm(n*length(beta),mean=0,sd=1), n, length(beta)) 
  U <- (exp(X %*% t(beta))) / (1 +exp(X %*% t(beta)))
  Y <- (runif(n) < U)*1
  out <- data.frame(Y, X)
  return(out)
}