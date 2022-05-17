# Implementing logistic regression in R

################################################################################
# Preparation

# Loading libraries
library(dplyr)

# Loading test data
testData <- read.csv("C:/Users/hammt/Documents/logReg/train.csv",
                     head = T, sep = ",", dec = ",")
# View(testData)

################################################################################
# Manipulation of test data

# Select variables for testing
str(testData)
testData <- testData %>% select(Survived, Pclass, Sex)
# View(testData)

# Define regressors
testData$Pclass <- as.numeric(testData$Pclass)
testData$Sex <- as.numeric(as.factor(testData$Sex))
regressors <- testData %>% select(Pclass, Sex)
# regressors <- as.matrix(regressors)

# Define dependent variable
depVar <- testData %>% select(Survived)
depVar <- as.matrix(depVar)

# Some Test Data
someNumber <- nrow(depVar)

set.seed(42)
someX0 <- c(rep(1, times = nrow(depVar)))
someX1 <- rnorm(someNumber)
someX2 <- rnorm(someNumber)
X <- matrix(c(someX0, someX1, someX2), ncol = 3)
xGLM <- matrix(c(someX1, someX2), ncol = 2)



################################################################################
# 1. Initilisation of maximum likelihood estimation of logit regression
################################################################################

#' The sigmoid function
#'
#' \code{sigm} is used to return an output between 0 and 1.
#'
#'
#' @param x Real valued matrix/vector.
#'
#' @examples
#' a <- -3
#' sigm(a)
#'
#'
#' @export
sigm <- function(x) {

  # Define sigmoid function
  s <- (exp(x) / (1 + exp(x)))

  return(s)
}


# The response function
p <- function(x, beta) {
  sigm(x %*% beta)
}


#' The log-likelihood function
#'
#' The \code{logLike} function is used to compute the log-likelihood in
#' logistic regression estimation.
#'
#'
#' @param x Matrix/vector containing the design matrix (including intercept).
#' @param y Matrix/vector containing the response variable.
#' @param beta Vector containing the coefficients of logistic regression.
#'
#' @examples
#' set.seed(1)
#' x1 <- rnorm(100, mean = 10);
#' x2 <- rnorm(100, mean = 10);
#' x0 <- c(rep(1, times = 100));
#' someVariable <- x1 - x2
#' x <- cbind(x0, x1, x2)
#' y <- rbinom(100, 1, exp(someVariable) / (1 + exp(someVariable)))
#' beta <- rnorm(ncol(x))
#' logLike(x, y, beta = beta)
#'
#' @export
logLike <- function(x, y, beta, ...) {
  sum(y * log(p(x, beta)) + (1 - y) * log(1 - p(x, beta)))
}


# The score function
score <- function(x, y, beta, ...) {

  # Deviation between true value and probality
  error <- y - p(x, beta)
  error <- as.matrix(error)

  # Rows of regressor matrix
  n <- nrow(x)

  # Columns of regressor matrix
  k <- ncol(x)

  # Define empty storage matrix for multiplication row by row
  rowProduct <- matrix(nrow = n, ncol = k)

  # Calculate product of each row
  for(i in 1:nrow(x)) {
    rowProduct[i, ] <- error[i] * x[i, ]
  }

  # Define empty storage vector
  colAddition <- c()

  # Summ columns of matrix to get the beta coefficients
  for(i in 1:ncol(rowProduct)) {
    colAddition[i] <- sum(rowProduct[, i])
  }
  return(colAddition)
}


# The fisher matrix
fisher <- function(x, p, ...) {
  vecProd <- as.vector(p * (1 - p))
  vecProd <- diag(vecProd)
  # Fisher Matrix
  f <- t(x) %*% vecProd %*% x
  return(f)
}


################################################################################
# Non-linear estimation: Gradient descent
################################################################################


# To find the vector of beta coefficients which maximizes the likelihood
# function the Gradient-descent algorithm is used. The structure of the
# code is obtained from the following website: https://towardsdatascience.com/
# implement-gradient-descent-in-python-9b93ed7108d1. The default values for
# maxIter and precision stem from this website: https://stat.ethz.ch/R-manual/
# R-patched/library/stats/html/glm.control.html.

#' The gradient descent algorithm
#'
#'
#' Gradient descent is an optimization algorithm used to compute the optimal
#' paramenters for a logistic regression model.
#'
#' The algorithm is applied in the \code{\link{logMod}} regression estimation
#' function.
#'
#' @param x Matrix/vector containing the design matrix (including intercept).
#' @param y Matrix/vector containing the response variable.
#' @param precision Degree of precision of algorithm.
#' @param iterMax Maximum number of iterations of the gradient descent
#' algorithm.
#'
#' @return A list with maximum likelihood estimation results for further
#'   computation.
#'
#' @references Hastie, T. J. and Pregibon, D. (1992): Generalized linear models.
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
#' fit <- gradDesc(x = x, y = y)
#'
#' @export
gradDesc <- function(x, y, maxIter = 25, precision = 1e-8) {

  # The iteration starts with a arbitrary vector of zero entries for beta
  beta <- rep(0, ncol(x))

  # Learning rate
  rate <- 1

  # Define an expression when iterations should stop
  convergence <- FALSE

  # Iteration counter
  i <- 0

  # Probability value +++
  pi <- sigm(x %*% beta)

  # Change in beta +++
  deltaBeta <- 1

  # Empty output vector for logLikelihood
  # logLikelihood <- c()

  # Gradient Descent is performed by while loop
  while(convergence == FALSE && i <= maxIter) {

    # Iteration count
    i <- i + 1
    iterations <- i

    # Store beta of previous iteration in prevBeta
    # prevBeta <- beta

    # Gradient descent formula with learning rate included
    betaNew <- beta + rate * solve(fisher(x, pi)) %*% score(x, y, beta)

    # +++
    piNew <- sigm(x %*% betaNew)

    # Define variable which tracks difference between beta and previous beta
    # deltaBeta <- betaNew - beta

    # Change in beta +++
    deltaBeta <- abs(betaNew - beta)

    # Update p +++
    beta <- betaNew
    pi <- piNew

    # Calcuate predicted values for dependet variable
    fittedValues <- x %*% beta

    # Compute the maximum of the log-Likelhood
    logLikelihood <- logLike(x, y, beta)

    # Update Matrix W
    W <- diag(pi * (1 - pi))



    # Check convergence of beta and break loop if convergence is realized
    if (sum(deltaBeta) < precision) {
      convergence <- TRUE
      break
    }

  }

  # Return beta coefficients that result from optimization procedure
  return(list(
    coefficients = beta,
    fittedValues = fittedValues,
    LogLikelihood = logLikelihood,
    pi = pi,
    W = W,
    Iterations = iterations))

  # Formulate output for non-convergence case
  if(convergence == FALSE) {
    stop(paste(i, "iterations reached without convergence. Increase iterMax?"))
  }

}


# Estimating the logLikelihood of the Null Model ----
logLikeNull <- function(x) {

  # Create the design matrix for the null model consisting out of one vector
  # with entries of value 1 only.
  xNull <-  as.matrix(rep(1, length(x)))

  # Apply the gradDesc() function to compute the coeefficients for this model
  restrMod <- gradDesc(xNull, x)

  # Store the computed coefficients in a new object
  betaNull <- restrMod$coefficients

  # Log-Likelihood computation
  logLikelihoodNull <- (sum((x * xNull %*% betaNull) -
                              (log(1 + exp(xNull %*% betaNull)))))

  # return result
  return(logLikelihoodNull)
}


# Estimating variance-covariance matrix
varCov <- function(x, pi) {

  # Calculate weight matrix with fitted values on the diagonal
  W <- diag(pi * (1 - pi))

  # Calculate variance-convariance matrix
  vcov <- solve(t(x) %*% W %*% x)

  # return result
  return(vcov)
}



################################################################################
# 2. S3
################################################################################


#' Logistic Regression Implementation
#'
#' \code{logMod} is used to fit logistic regression models with a binaray
#' response variable.
#'
#' @param formula an object of class "formula": a symbolic description of the
#' model to be fitted.
#' @param data an optional data frame, list or environment containing the
#' variables in the model. If the variables are not found in \code{data}, the
#' varialbes are taken from \code{environment(formula)}.
#'
#'
#' @return \code{logitModel} returns an object of \code{\link[base]{class}}
#'     \emph{logitModel}, .
#'
#' @examples
#'
#' @export
logMod <- function(x, ...) UseMethod("logMod")

logMod.default <- function(formula, data = list(), ...) {

  # Model.frame() creates a dataframe, where the first column
  # always is dependent variable
  mf <- model.frame(formula = formula, data = data)

  # Constructs a design matrix containing all regressors
  x <- model.matrix(attr(mf, "terms"), data = mf)

  # Constructs a numeric vector with the response variable
  y <- model.response(mf)

  # Make sure response variable only contains 0s and 1s
  if (!(0 %in% y && 1 %in% y)) {
    y <- factor(y, labels = c(0, 1))
  }

  # Make sure that dependent variable is binary
  if (length(unique(y)) != 2) {
    stop("Response variable is expected to be binary")
  }

  # Using optimization algorithm to perform logistic regression
  res <- gradDesc(x, y, ...)

  # Store entered formula
  res$formula <- formula

  # Call function via match.call()
  res$call <- match.call()

  # Extract model variables and store them
  res$model <- matrix(c(y, x), ncol = ncol(x) + 1)

  # Variance-covariance matrix
  pi <- as.numeric(res$pi)
  res$vCov <- varCov(x, pi)

  # Calculate degrees of freedom
  degFree <- nrow(x) - ncol(x)
  res$degFree <- degFree

  # Calculate degrees of freedom for Null model
  nullFree <- nrow(x) - 1
  res$nullFree <- nullFree

  # Compute null deviance
  LogLikelihoodNull <- logLikeNull(y)
  res$nullDeviance <- - 2 * LogLikelihoodNull

  # # Compute deviance residuals
  c <- ifelse(y == 0, - 1, y)
  residuals <- as.numeric(c * sqrt(- 2 * ((y * res$fittedVal) -
                                            (log(1 + exp(res$fittedVal))))))
  residSum <- sum(residuals ^ 2)
  res$residSum <- residSum
  res$residuals <- residuals

  # Compute Akaike Information Criterion (AIC)
  AIC <- 2 * ncol(x) - 2 * res$LogLikelihood
  res$AIC <- AIC

  # Define class of list so that
  class(res) <- "logMod"

  # return results
  return(res)
}


# Defining print method for logMod() ----

print.logMod <- function(x, ...) {
  cat("Call:\t")
  print(x$call)
  cat("\nCoefficients:\n")
  cat(t(round(x$coefficients, digits = 5)), "\n")
  cat("\nDegrees of Freedom:", x$nullFree, "Total (i.e. Null);", x$degFree,
      "\tResidual")
  cat("\nNull Deviance:\t", round(x$nullDeviance), "\n")
  cat("Residual Deviance:", round(x$residSum), "\tAIC:", round(x$AIC))
}



# The summary for an object of class "logMod" ----
summary.logMod <- function(x, ...) {

  # Overview table for residuals of logistic regression
  residQuant <- matrix(quantile(x$residuals))
  x$residQuant <- t(residQuant)

  # Column names for residual quantiles
  colnames(x$residQuant) <- c("Min", "1Q", "Median", "3Q", "Max")

  # Standard Error
  stdError <- sqrt(diag(x$vCov))
  x$stdError <- stdError

  # z-values
  zValue <- x$coefficients / stdError
  x$zValue <- zValue

  # p-values
  pValue <- 2 * pnorm(- abs(zValue))
  x$pValue <- pValue

  # Coefficient matrix including stats
  x$coefficients <- cbind(coef(x),
                          stdError,
                          zValue,
                          pValue)

  # Column names for coefficient table
  colnames(x$coefficients) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")

  # define class
  class(x) <- "summary.logMod"

  # return list
  x
}


# Print Function for summary.logMod() ----
print.summary.logMod <- function(x, ...) {
  cat("Call:\t")
  print(x$call)
  cat("\nDeviance Residuals:\n")
  print(x$residQuant)
  cat("\nCoefficients:\n")
  printCoefmat(x$coefficients, P.value =  TRUE, has.Pvalue = TRUE, digits = 4L)
  cat("\nNull deviance:", round(x$nullDeviance, digits = 1), "on", x$nullFree,
      "degrees of freedom")
  cat("\nResidual deviance:", round(x$residSum, digits = 1), "on",
      "degrees of freedom")
  cat("\nAIC:", round(x$AIC, digits = 1))
  cat("\n")
  cat("\nNumber of Fisher Scoring iterations:", x$Iterations)

}

################################################################################
# Plot Functions

plot.logMod <- function(x, ...) {


  # Plot 1 ----

  plot(x$pi, x$residuals,
       main = "Residuals vs Fitted",
       xlab = paste("Predicted Values\n", deparse(x$formula)),
       ylab = "Residuals")

  # Add horiztontal grey line to plot, which represents y = 0
  abline(a = 0, b = 0, lty = 3, col = "gray")

  # Add smooth curve
  lines(lowess(x = x$pi, y = x$residuals), col = "red")



  # Plot 2

  p <- qqnorm(x$residuals,
              main = "Normal Q-Q",
              xlab = paste("Theoretical Quantiles\n", deparse(x$formula)),
              ylab = "Std. deviance resid.")

  qqline(x$residuals, lty = 3, col = "gray")


  # Plot 3

  X <- x$model[, -1]
  prediction <- X %*% x$coefficients

  stdDevRes <- sqrt((x$residuals)^2)
  ylab <- as.expression(substitute(sqrt(abs(x)),
                                   list(x = as.name("Std. Deviance Resid."))))

  plot(prediction, stdDevRes,
       main = "Scale Location",
       xlab = paste("Predicted Values\n", deparse(x$formula)),
       ylab = ylab)

  # Plot Smoothing via LOWESS regression
  lines(lowess(x = x$fittedValues,
               y = sqrt(abs(x$residuals))), col = "red")


  # # Plot 4

  # y <- as.matrix(x$model[, 1])
  # X <- x$model[, -1]
  #
  # leverage <- diag(sqrt(x$W) %*% X %*%
  #                     (solve(t(X) %*% x$W %*% X)) %*%
  #                      t(X) %*% sqrt(x$W))
  # pearsonResidual <- (y - x$fittedValues) /
  #                      sqrt(x$fittedValues * (1 - x$fittedValues))
  #
  # plot(x = leverage, y = pearsonResidual,
  #       main = "Residual vs Leverage",
  #       ylab = "Std. Pearson resid.",
  #       xlab = paste("Leverage\n", deparse(model$formula)),
  #       ylim = range(pearsonResidual) * 1.1, ...)
  #
  #  abline(a = 0, b = 0, lty = 3, col = "gray")
  #
  # # Plot Smoothing via LOWESS regression
  # lines(lowess(x = leverage,
  #               y = pearsonResidual), col = "red",
  #        xlab = "")



  # Save plot objects in a list and print each individually
  # plots <- list(plot1, plot2)
  # for(i in 1:length(plots)) {
  #   print(plots[[i]])
  #
  # }
  #

  # print(c(plot1, plot2))


}



# col = col[1 + x$pi], ...




















