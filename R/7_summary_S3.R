################################################################################
# 4. The Summary Method for the LogMod() Function and a respective Print Method
################################################################################

# The function summary.logMod() makes sure that objects of class logMod are
# recognized by the function summary() of the base package. The input of the
# function x should be an object of class logMod.


#' S3 Summary Method for logMod Objects
#'
#' An internal function, which defines a \code{\link{summary}} method for
#' an object of class \code{logMod}. The output of this function gives further
#' statistics that inform about the quality of the estimation results and
#' that might be used for model comparison.
#'
#' @param x a logMod object for which a summary should be returned.
#' @param ... additional arguments affecting the summary produced.
#'
#' @return a list with statistics summarazing the model estimation results.
#'
#'
#' @export
summary.logMod <- function(x, ...) {

  # Overview table for the residuals of the logistic regression
  residQuant <- matrix(quantile(x$residuals))
  x$residQuant <- t(residQuant)

  # Column names for the residual quantiles
  colnames(x$residQuant) <- c("Min", "1Q", "Median", "3Q", "Max")

  # The standard Error
  stdError <- sqrt(diag(x$vCov))
  x$stdError <- stdError

  # The z-values
  zValue <- x$coefficients / stdError
  x$zValue <- zValue

  # The p-values
  pValue <- 2 * pnorm(- abs(zValue))
  x$pValue <- pValue

  # The coefficient matrix including stats
  x$coefficients <- cbind(coef(x),
                          stdError,
                          zValue,
                          pValue)

  # The column names for the coefficient table
  colnames(x$coefficients) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")

  # Define the class
  class(x) <- "summary.logMod"

  # return list
  x
}


#' S3 Print Method for summary.logMod Objects
#'
#' An internal function, which defines a \code{\link{print}} method for an
#' object of class \code{\link{summary.logMod}}.
#'
#'
#' @param x an object of class "summary.logMod".
#' @param ... optional arguments to \code{summary} methods.
#'
#' @return a summary of the estimation results.
#'
#' @export
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
