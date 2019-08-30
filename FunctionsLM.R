# Generate n-dimensional response Y that follows linear regression model Y = Xbeta + epsilon, where epsilon is normal zero with variance sigma^2 independent across samples. Seed should be set at the beginning of the function
# X - design matrix
# beta - given parameter vector
# sigma - standard deviation of the noise
# seed  - starting seed value
generateY <- function(X, beta, sigma, seed = 5832652){
  #[ToDo] Set seed and generate Y following linear model
  Y=numeric(0)
  for(i in 1:n){
    
    # calculating means for each data point
    u=beta%*%X[i,]
    
    # generating normal Y
    Y=c(Y,rnorm(1, mean = u,sd = sigma))
  # Return Y
  return(Y)
  }
}

# Calculate beta_LS - least-squares solution, do not use lm function
# X - design matrix
# Y -response
calculateBeta <- function(X, Y){
  # Calculate beta_LS
  # Use well known OLS formula
  X1=t(X)
  beta_LS=solve(X1%*%X,X1%*%Y)
  # Return beta
  return(beta_LS)
}

# Calculate MSE
calculateMSE <- function(beta, beta_LS){
  MSE=crossprod(beta-beta_LS)
  # Return MSE - error ||beta - beta_LS||_2^2
  return(MSE)
}