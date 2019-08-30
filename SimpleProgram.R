# Generate data from linear regression model and calculate the least squares vector of coefficients
#####################################################################################################

# Model parameters
n = 100 # sample size
p = 10 # number of covariates
sigma = 2 # noise standard deviation
beta = rep(2,p) # true vector of coefficients
X = matrix(rnorm(n*p), n, p) # n by p matrix of predictors

# [ToDo] Use generateY function to generate Y
generateY=function(X, beta, sigma, seed = 5832652){
  Y=numeric(0)
  for(i in 1:n){
    
    # calculating means for each data point
    u=beta%*%X[i,]
    
    # generating normal Y
    Y=c(Y,rnorm(1, mean = u,sd = sigma))
  }
  return(Y)
}

# [ToDo] Use calculateBeta function to calculate beta_LS
calculateBeta=function(Y,X){
  
  # use well-known OLS formula
  X1=t(X)
  beta_LS=solve(X1%*%X,X1%*%Y)
  
  return(beta_LS)
}

# [ToDo] Use calculateMSE to assess the estimation error measured by squared eucledian distance - ||beta - beta_LS||_2^2
calculateMSE=function(beta, beta_LS){
  MSE=crossprod(beta-beta_LS)
  return(MSE)
}

############################################################################################

# running the functions

Y=generateY(X, beta, sigma, seed = 5832652)
Y
beta_LS=calculateBeta(Y,X)
beta_LS
MSE=calculateMSE(beta,beta_LS)
MSE




