################################################################################
# 2.2 Computing the Variance-Covariance Matrix Separatly (Compare fisher())
################################################################################

# To compute the variance-covariance matrix separatly the varCov() is created.
# The output of this function is used a subsequent step to compute the
# standard error of the coefficients. The inputs of the function are the
# design matrix x and the probabilty pi, which is the output of the response
# function.
varCov <- function(x, pi) {

  # Calculate weight matrix with the probability pi on the diagonal
  W <- diag(pi * (1 - pi))

  # Calculate variance-convariance matrix
  vcov <- solve(t(x) %*% W %*% x)

  return(vcov)
}
