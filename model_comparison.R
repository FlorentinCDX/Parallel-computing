#choose the best model according to the R-squared criteria
basic.modelcomparison_Rsqared <- function(x, y, set.of.beta) {
  mt_R_squared <- matrix(0, 1, dim(set.of.beta)[1])
  total_var <- sum((y-mean(y))^2)
  for(i in 1:dim(set.of.beta)[1]) {
    u_pred <- (exp(x %*% set.of.beta[i,])) / (1 +exp(x %*% set.of.beta[i,])) 
    y_pred <- (runif(length(y)) < u_pred)*1
    explained_var <- sum((y_pred - mean(y))^2)
    mt_R_squared[i] <- 1 - (explained_var/total_var)
  }
  nb_model <- which.max(mt_R_squared)
  return(list(R_squared_best_model = mt_R_squared[nb_model], best_model = set.of.beta[nb_model,]))
}

#choose the best model according to the AIC criteria
basic.modelcomparison_AIC <- function(x, y, set.of.beta) {
  mt_AIC <- matrix(0, 1, dim(set.of.beta)[1])
  K <-dim(set.of.beta)[2]
  for(i in 1:dim(set.of.beta)[1]) {
    mt_AIC[i] <- -2*loglikl(x, y, set.of.beta[i,]) + 2*K + (2*K*(K+1)/(length(y)-K-1)) #Corrected AIC for K small <40
  }
  nb_model <- which.min(mt_AIC)
  return(list(AIC_best_model = mt_AIC[nb_model], best_model = set.of.beta[nb_model,]))
}

#choose the best model according to the cross validation error
basic.modelcomparison_CV <- function(x, y, set.of.beta) {
  mt_CV <- matrix(0, 1, dim(set.of.beta)[1])
  for(i in 1:dim(set.of.beta)[1]) {
    mt_CV[i] <- basiv.cv(x,y, beta0)
  }
  nb_model <- which.min(mt_CV)
  return(list(CV_best_model = mt_CV[nb_model], best_model = set.of.beta[nb_model,]))
}

#choose the best model according to a subset of the relevant variables, allow to choose the criteria thancks to the term selection
basic.modelcomparison <- function(x, y, models, beta0, selection = "all") {
  if(length(beta0) != ncol(x) | ncol(models) != ncol(x)){return("The number of values in beta should be equal to the number of variable of X")} else {
  bet <- matrix(0, ncol = ncol(x), nrow=dim(models)[1])
  for(i in 1:dim(models)[1]){
    combi = models[i,]
    betaa= rhapson_newton(x, y, beta0*combi, 0.1)
    bet[i,] = combi
  }
  if(selection == "AIC"){basic.modelcomparison_AIC(x, y, bet)}
  else if(selection == "Rsqared"){basic.modelcomparison_Rsqared(x, y, bet)}
  else if(selection == "cv"){basic.modelcomparison_CV(x, y, bet)}
  else if(selection == "all"){list(AIC_model = basic.modelcomparison_AIC(x, y, bet),
                                   Rsqared_model = basic.modelcomparison_Rsqared(x, y, bet),
                                   CV_model = basic.modelcomparison_CV(x, y, bet))}
  else{return("You have to choose a selection method between CV, AIC or R-squared")} }
}
