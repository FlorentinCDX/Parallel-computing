source("Data_generation.R")
source("basic_mle.R")
source("cross_validation_loo.R")
source("model_comparison.R")
source("model_selection.R")

beta <- matrix(c(0.6, -0.01, -2.5, 12, 0.05), 1, 5)
data <- rlogit(1000,beta); data

Y <- data[,1]
X <- data[,2:dim(data)[2]]
x <- as.matrix(X)
y <- as.vector(Y)
beta <- t(beta)

profvis::profvis({loglikl(x, y, beta)})
loglikl(x, y, beta)
gradient(x, y, beta)
hessian(x, y, beta)
trc= as.vector(hessian(x, y, beta))
dim(trc) = c(5,5)
as.matrix(trc , 5, 5, byrow = TRUE)
#execption management
loglikl(x, y, beta[-1])
gradient(x, y, beta[-1])
hessian(x, y, beta[-1])

beta0 = c(1,0.5,-2,11.5,0.5)
rhapson_newton(x, y, beta0,0.1)
rhapson_newton(x, y,  beta0[-1],0.1)

profvis::profvis(basiv.cv(x,y, beta0))

set.of.beta <- matrix(c(0.6, -0.01, -2.5, 12, 0.05), 10, 5, byrow = T) +  matrix(rnorm(5*10,mean=0,sd=2), 10, 5)
profvis::profvis(basic.modelcomparison_Rsqared(x,y, set.of.beta))
profvis::profvis(basic.modelcomparison_AIC(x,y, set.of.beta))
profvis::profvis(basic.modelcomparison_CV(x,y, set.of.beta))

model1 = c(1,1,1,0,0)
model2 = c(0,1,0,1,0)
models = rbind(model1, model2)

basic.modelcomparison(x, y, models, beta0)
basic.modelcomparison(x, y, models, beta0, selection = "AIC")
basic.modelcomparison(x, y, models, beta0, selection = "Rsqared")
basic.modelcomparison(x, y, models, beta0, selection = "cv")
#execption management
basic.modelcomparison(x, y, models, beta0, selection = "")
basic.modelcomparison(x, y, models[,-1], beta0[-1])

profvis::profvis(basic.modelselection(x, y, "forward", beta0))
basic.modelselection(x, y, "backward", beta0)$CV_model
basic.modelselection(x, y, "baCKward", beta0)$CV_model
#execption management
basic.modelselection(x, y, "test", beta0)
basic.modelselection(x, y, "forward", beta0[-1])


set.seed(1)
beta <- matrix(c(0.6, -0.01, -2.5, 12, 0.05), 1, 5)
data <- rlogit(100,beta); data
Y <- data[,1]
X <- data[,2:dim(data)[2]]
x <- as.matrix(X)
y <- as.vector(Y)
beta <- t(beta)
basic.modelcomparison(x, y, models, beta0)$AIC_model
basic.modelcomparison(x, y, models, beta0, selection = "AIC")
