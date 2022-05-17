################################################################################
# 1. Initilisation of Maximum Likelihood Estimation of Logit Regression
################################################################################

# The following functions, sigm(), p(), logLike(), score() and fisher(), are
# ingredients of the gradient descent algorithm, which is used to
# find a coefficient vector, which maximizes the likelihood function.


# 1. The sigmoid function transforms a real-valued input x into an output, which
# ranges from 0 to 1.
sigm <- function(x) {

  s <- (exp(x) / (1 + exp(x)))

  return(s)
}


# 2. The response function uses the sigmoid function to transform the model
# consisting out of the design matrix x and a coefficient vector beta into
# an output ranging from 0 to 1.
p <- function(x, beta) {

  p <- sigm(x %*% beta)

  return(p)
}


# 3. The logLike() function computes the log-likelihood.
logLike <- function(x, y, beta, ...) {

  l <- sum(y * log(p(x, beta)) + (1 - y) * log(1 - p(x, beta)))

  return(l)
}


# 4. The function score() computes the the score vector, which is the first
# derivative of the likelihood function.
score <- function(x, y, beta, ...) {

  # Deviation between true value y and the observed probality computed by p()
  error <- y - p(x, beta)
  error <- as.matrix(error)

  # Store nummber of rows of the regressor matrix
  n <- nrow(x)

  # Store number of columns of the regressor matrix
  k <- ncol(x)

  # Define empty storage matrix for row by row multiplication
  rowProduct <- matrix(nrow = n, ncol = k)

  # Calculate product of each row with a for loop
  for(i in 1:nrow(x)) {
    rowProduct[i, ] <- error[i] * x[i, ]
  }

  # Define empty storage vector for the subsequent summation of the columns
  # of the matrix produced by the previous for loop
  colAddition <- c()

  # Sum columns of matrix by for loop to get the beta coefficients
  for(i in 1:ncol(rowProduct)) {
    colAddition[i] <- sum(rowProduct[, i])
  }

  return(colAddition)
}


# 5. The function fisher() computes the fisher matrix, which is the result of
# the second derivative of the likelihood function.
fisher <- function(x, p, ...) {

  # Calculation of weighting matrix
  vecProd <- as.vector(p * (1 - p))
  vecProd <- diag(vecProd)

  # Fisher Matrix
  f <- t(x) %*% vecProd %*% x

  return(f)
}

