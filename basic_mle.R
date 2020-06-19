# Here is the implementation of the besic.mle from scratch.

# Define the negative log likelihood function
loglikl <- function(x,y,beta) {
  if(length(beta) != ncol(x)){return("The number of values in beta should be equal to the number of variable of X")} else{
  if(length(beta)==1){
    u = exp(x * beta) / (1 + exp(x * beta))
    l=0
    for (i in 1:length(y)) {
      l = l - (y[i]*log(u[i]) + (1-y[i])*log(1-u[i]))
    }}
  else{
    u = exp(crossprod(x,beta)) / (1 + exp(crossprod(x,beta)))
    l=0
    for (i in 1:nrow(x)) {
      l = l - (y[i]*log(u[i]) + (1-y[i])*log(1-u[i]))
    }}
  return(l) }
}

# Define the gradient function

gradient = function(x,y,beta) {
  if(length(beta) != ncol(x)){return("The number of values in beta should be equal to the number of variable of X")} else{
  if(length(beta)==1){
    u = exp(x * beta) / (1 + exp(x * beta))
    for (i in 1:length(test)) {g = g + test[i]*(u[i]-y[i])}
  }else{
    g= rep(0,ncol(x))
    u = exp(crossprod(x,beta)) / (1 + exp(crossprod(x,beta)))
    for (i in 1:nrow(x)) {g = g + x[i,]*(u[i]-y[i])}
  }
  return(g) }
}

gradient = function(x,y,beta) {
  if(length(beta) != ncol(x)){
    return("The number of values in beta should be equal to the number of
           variable of X")}
  else{
    if(length(beta)==1){
      u = exp(x * beta) / (1 + exp(x * beta))
      for (i in 1:length(test)) {g = g + test[i]*(u[i]-y[i])}
    }else{
      g= rep(0,ncol(x))
      u = exp(x %*% beta) / (1 + exp(x %*% beta))
      for (i in 1:nrow(x)) {g = g + x[i,]*(u[i]-y[i])}
    }
    return(g) }
}

gradient_new <-  function(x,y,beta) {
  if(length(beta) != ncol(x)){return("The number of values in beta should 
                                      be equal to the number of variable of X")} else{
                                        if(length(beta)==1){
                                          u = exp(x * beta) / (1 + exp(x * beta))
                                          for (i in 1:length(test)) {g = g + test[i]*(u[i]-y[i])}
                                        }else{
                                          g= rep(0,ncol(x))
                                          u = exp(crossprod(t(x),beta)) / (1 + exp(crossprod(t(x),beta)))
                                          vec = parSapply(cl, 1:nrow(x),function(i){x[i,]*(u[i]-y[i])})
                                        }
                                        return(as.matrix(rowSums(vec))) }
}

# Define the Hersien function

hessian = function(x,y,beta) {
  if(length(beta) != ncol(x)){
    return("The number of values in beta should be equal to the number of 
           variable of X")}
  else{
    if(length(beta)==1){
      u = exp(x * beta) / (1 + exp(x * beta))
      h = u[i]*(1-u[i]) * t(x) %*% x}
    else{
      u = exp(x %*% beta) / (1 + exp(x %*% beta))
      h = matrix(0,ncol(x),ncol(x))
      for (i in 1:nrow(x)) {
        h = h + u[i]*(1-u[i]) * t(x) %*% x}
    }
    return(h)}
}

hessian = function(x,y,beta) {
  if(length(beta) != ncol(x)){
    return("The number of values in beta should be equal to the number of 
           variable of X")}
  else{
    if(length(beta)==1){
      u = exp(x * beta) / (1 + exp(x * beta))
      h = u[i]*(1-u[i]) * t(x) %*% x}
    else{
      u = exp(crossprod(t(x),beta))  / (1 + exp(crossprod(t(x),beta)) )
      h = matrix(0,ncol(x),ncol(x))
      for (i in 1:nrow(x)) {
        h = h + u[i]*(1-u[i]) * crossprod(x,x)}
    }
    return(h)}
}


hessian_new <-  function(x,y,beta) {
  if(length(beta) != ncol(x)){return("The number of values in beta should be equal to the number of variable of X")} else{
    if(length(beta)==1){
      u = exp(crossprod(x,beta))  / (1 + exp(crossprod(x,beta)) )
      h = u[i]*(1-u[i]) * crossprod(x, x)}
    else{
      u =exp(crossprod(t(x),beta))  / (1 + exp(crossprod(t(x),beta)) )
      vec = parSapply(cl, 1:nrow(x),function(i){(u[i]*(1-u[i]))*crossprod(x, x)})
      vec =matrix(rowSums(vec), ncol=ncol(x), nrow = ncol(x))
      return(vec)}}
}
  

# Define the Newton-Raphson function

rhapson_newton <- function(x,y,betaO,eps) {
  if(length(beta0) != ncol(x)){return("The number of values in beta should be equal to the number of variable of X")} else {
  beta = betaO
  if(length(beta)==1){
    dir = eps+1
    while(sqrt(dir)^2>eps)  {
      grd= gradient(x,y,beta)
      hes= hessian(x,y,beta)
      dir=(1/hes)*grd 
      beta = beta - dir
    }}
  else{
    dir = rep(eps+1,length(beta))
    while(norm_vec(dir)>eps)  {
      grd= gradient(x,y,beta)
      hes= hessian(x,y,beta)
      dir = crossprod(solve(hes), grd)
      beta = beta - dir
    }}
  return(beta) }
}

norm_vec <- function(x) sqrt(sum(x^2))
