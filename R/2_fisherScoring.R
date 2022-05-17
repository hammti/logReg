################################################################################
# 2. Non-Linear Estimation: Fisher Scoring Algorithm
################################################################################


# To find the vector of beta coefficients which maximizes the log-likelihood
# function the Fisher scoring algorithm is used. The structure of the
# code is obtained from the following website: https://towardsdatascience.com/
# implement-gradient-descent-in-python-9b93ed7108d1. The default values for
# maxIter and precision stem from this website: https://stat.ethz.ch/R-manual/
# R-patched/library/stats/html/glm.control.html.


#' The Fisher Scoring algorithm
#'
#' Fisher scoring is an optimization algorithm used to compute the
#' paramenters for a logistic regression model.
#'
#' The algorithm is applied in the \code{\link{logMod}} regression estimation
#' function.
#'
#' @param x Matrix/vector containing the design matrix (including intercept).
#' @param y Matrix/vector containing the response variable.
#' @param precision Degree of precision of the Fisher scoring algorithm.
#' @param maxIter Maximum number of iterations of the Fisher scoring
#' algorithm.
#'
#' @return
#' A list with maximum likelihood estimation results for further computation.
#'
#'
#' @references
#' Hastie, T. J. and Pregibon, D. (1992): Generalized linear models.
#' Statistical Models in S, J. M. Chambers and T. J. Hastie (eds), USA:
#' Wadsworth & Brooks/Cole.
#'
#'
#'
#' @examples
#' set.seed(1)
#' x1 <- rnorm(100, mean = 10);
#' x2 <- rnorm(100, mean = 10);
#' x0 <- c(rep(1, times = 100));
#' someVariable <- x1 - x2
#' x <- cbind(x0, x1, x2)
#' y <- rbinom(100, 1, exp(someVariable) / (1 + exp(someVariable)))
#' fit <- fishScore(x = x, y = y)
#'
#' @export
fishScore <- function(x, y, maxIter = 25, precision = 1e-8) {

  # The optimization procedure starts with a arbitrary vector of zero entries
  # for beta
  beta <- rep(0, ncol(x))

  # The learning rate represents the step length of the optimization procecure
  rate <- 1

  # Define a Boolean convergence expression, which is FALSE at the beginning
  # of the optimization procedure
  convergence <- FALSE

  # Define an objects, which counts the number of iterations until the
  # optimization procedure converges
  i <- 0

  # Define the probability pi, which stores the probality (model output
  # of logit regression) for each observation after each iteration
  pi <- sigm(x %*% beta)

  # Define an object, which keeps track of the difference between the previous
  # coefficient vector and the new coefficient vector
  deltaBeta <- 1

  # The Fisher scoring algorithm as such is performed by a while loop. The
  # loop stops when convergence and/or the number of predefined maximum
  # iterations are reached
  while(convergence == FALSE && i <= maxIter) {

    # Update iteration count
    i <- i + 1
    iterations <- i

    # The function to be optimized by the algorithm so that the likelihood
    # is maximimized
    betaNew <- beta + rate * solve(fisher(x, pi)) %*% score(x, y, beta)

    # Calculate the new probability
    piNew <- sigm(x %*% betaNew)

    # Store the absolute change in beta
    deltaBeta <- abs(betaNew - beta)

    # Update beta and pi
    beta <- betaNew
    pi <- piNew

    # Calcuate fitted values for the model (notice: this is not the output of
    # the response function. Accordingly, the values are not necessarilty
    # between 0 and 1)
    fittedValues <- x %*% beta

    # Compute the maximum of the log-Likelhood
    logLikelihood <- logLike(x, y, beta)

    # Update Matrix W (compare fisher())
    W <- diag(pi * (1 - pi))

    # Check convergence of beta and break loop if convergence is realized
    if (sum(deltaBeta) < precision) {
      convergence <- TRUE
      break
    }

  }

  # Return beta coefficients that result from optimization procedure in a list
  return(list(
    coefficients = beta,
    fittedValues = fittedValues,
    LogLikelihood = logLikelihood,
    pi = pi,
    W = W,
    Iterations = iterations))

  # Formulate output expression for case of non-convergence
  if(convergence == FALSE) {
    stop(paste(i, "No convergence was reached after iterations.
               Increase maxIter?"))
  }

}

