#Here our aim is to implement a function that takes as input argument the sample and the direction (backward or forward). 
#This returns the best model and its estimator of the prediction error.

basic.modelselection <- function(x, y, direction = "forward", beta0, selection = "all"){
  if(length(beta0) != ncol(x)){return("The number of values in beta should be equal to the number of variable of X")} else {
  if(tolower(direction)=="forward"){
    # Select the best model for all 1 variables models (Identity matrix) 
    models <- diag(dim(x)[2])
    best_model <- basic.modelcomparison(x, y, models, beta0, selection)$best_model
    
    # Select the best model between combinations of p variables and the previously selected best model 
    for(p in 2:ncol(x)){
      models <- matrix(0, ncol=ncol(x), nrow=choose(ncol(x),p))
      while( all(rowMeans(models)==rep( mean(c(rep(1,p),rep(0,ncol(x)-p))),ncol(x)))==FALSE ){
        for(i in 1:nrow(models)){
          combin <- sample(1:ncol(x),p)
          row <- rep(0,ncol(x))
          for(j in combin){row[j]<-1}
          ajouter<- TRUE
          for(k in 1:nrow(models)){ if( all(row==models[k,]) ){
            ajouter <- FALSE
            break}}
          if(ajouter==TRUE){  for(m in 1:nrow(models)){if( all(models[m,]==rep(0,ncol(X))) ){ 
            models[m,]<- row
            break}} }
        } }
      models = rbind(models, unlist(best_model, use.names=FALSE))
      best_model <- basic.modelcomparison(x, y, models, beta0, selection)
      return(best_model)
    } }
  else if(tolower(direction)=="backward"){
    # Select the best model for all variables models  
    models <- matrix(1, ncol=ncol(x), nrow=1)
    best_model <- basic.modelcomparison(x, y, models, beta0, selection)$best_model
    # Select the best model between combinations of p variables and the previously selected best model 
    for(p in ncol(x):1){
      models <- matrix(0, ncol=ncol(x), nrow=choose(ncol(x),p))
      while( all(rowMeans(models)==rep( mean(c(rep(1,p),rep(0,ncol(x)-p))),ncol(X)))==FALSE ){
        for(i in 1:nrow(models)){
          combin <- sample(1:ncol(x),p)
          row <- rep(0,ncol(x))
          for(j in combin){row[j]<-1}
          ajouter<- TRUE
          for(k in 1:nrow(models)){ if( all(row==models[k,]) ){
            ajouter <- FALSE
            break}}
          if(ajouter==TRUE){  for(m in 1:nrow(models)){if( all(models[m,]==rep(0,ncol(x))) ){ 
            models[m,]<- row
            break}} }
        } }
      models = rbind(models, unlist(best_model, use.names=FALSE))
      best_model <- basic.modelcomparison(x, y, models, beta0, selection)
      return(best_model)
    }
  }
  else {return("Please enter backward or forward as direction in the function ")} }
}
