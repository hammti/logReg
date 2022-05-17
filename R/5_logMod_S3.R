################################################################################
# 3. The Function logMod() and its Print Method
################################################################################

# Based on the function fishScore() the logMod() function is defined in the
# following making use of S3 classes. This includes the definition of a
# print method for logMod().


#' Estimating logistic regression model
#'
#' \code{logMod} is used to fit logistic regression models with a binaray
#' response variable.
#'
#' @param formula an object of class "formula": represents the
#' model to be fitted.
#' @param data an optional data frame, list or environment containing the
#' variables in the model. If the variables are not found in \code{data}, the
#' variables are taken from \code{environment(formula)}.
#' @param ... optional specifications used for the setup of the Fisher scoring
#' algorithm (see \code{\link{fishScore}}).
#'
#'
#' @return \code{logMod} returns an object of \code{\link[base]{class}}
#'     \emph{logMod}.
#'
#' @examples
#' set.seed(1)
#' x1 <- rnorm(100, mean = 10);
#' x2 <- rnorm(100, mean = 10);
#' someVariable <- x1 - x2
#' x <- cbind(x1, x2)
#' y <- rbinom(100, 1, exp(someVariable) / (1 + exp(someVariable)))
#' fit <- logMod(y ~ x)
#'
#' @export


# Define default function for logMod()
logMod <- function(formula, data = list(), ...) {

  # Model.frame() creates a dataframe, where the first column
  # always is dependent variable to be estimated
  mf <- model.frame(formula = formula, data = data)

  # Constructs a design matrix containing all regressors of the respecitve model
  x <- model.matrix(attr(mf, "terms"), data = mf)

  # Constructs a numeric vector containing the response variable
  y <- model.response(mf)

  # Make sure that the response variable only contains entries of 0s and 1s
  if (!(0 %in% y && 1 %in% y)) {
    y <- factor(y, labels = c(0, 1))
  }

  # Make sure that the dependent variable is binary, otherwise print an error
  # message
  if (length(unique(y)) != 2) {
    stop("Response variable has to be binary.")
  }

  # Using optimization algorithm to estimate the maximum likelihood estimates
  # (among others)
  result <- fishScore(x, y, ...)

  # Store entered formula
  result$formula <- formula

  # Call function via match.call()
  result$call <- match.call()

  # Extract model variables and store them
  result$model <- matrix(c(y, x), ncol = ncol(x) + 1)

  # Compute variance-covariance matrix with the function varCov
  pi <- as.numeric(result$pi)
  result$vCov <- varCov(x, pi)

  # Calculate the degrees of freedom for the estimated model
  degFree <- nrow(x) - ncol(x)
  result$degFree <- degFree

  # Calculate degrees of freedom for the Null model
  nullFree <- nrow(x) - 1
  result$nullFree <- nullFree

  # Compute the null deviance based on logLikeNull()
  LogLikelihoodNull <- logLikeNull(y)
  result$nullDeviance <- - 2 * LogLikelihoodNull

  # # Compute the deviance residuals
  c <- ifelse(y == 0, - 1, y)
  residuals <- as.numeric(c * sqrt(- 2 * ((y * result$fittedVal) -
                                            (log(1 + exp(result$fittedVal))))))
  residSum <- sum(residuals ^ 2)
  result$residSum <- residSum
  result$residuals <- residuals

  # Compute Akaike Information Criterion (AIC)
  AIC <- 2 * ncol(x) - 2 * result$LogLikelihood
  result$AIC <- AIC

  # Define class of list so that
  class(result) <- "logMod"

  return(result)
}


#' S3 Print Method for logMod Objects
#'
#' An internal function, which defines a \code{\link{print}} method for an
#' object of class \code{logMod}.
#'
#'
#' @param x a logMod object.
#' @param ... optional arguments to \code{print} methods.
#'
#' @return a short summary of the estimation results.
#'
#' @export
print.logMod <- function(x, ...) {
  cat("Call:\t")
  print(x$call)
  cat("\nCoefficients:\n")
  print(t(round(x$coefficients, digits = 5)))
  cat("\nDegrees of Freedom:", x$nullFree, "Total (i.e. Null);", x$degFree,
      "\tResidual")
  cat("\nNull Deviance:\t", round(x$nullDeviance), "\n")
  cat("Residual Deviance:", round(x$residSum), "\tAIC:", round(x$AIC))
}
