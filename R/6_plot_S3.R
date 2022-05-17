################################################################################
# 5. The Plot() Method for LogMod()
################################################################################

# To get visualizations of the results of the logistic regression perfomed by
# the logMod() function a plot method is written in the following. With the
# help of plot.logMod the plot() function of the base package should recognize
# objects x of class logMod.


#' S3 Plot Method for logMod Objects
#'
#' An internal function, which defines a \code{\link{plot}} method for an object
#' of class \code{logMod}.
#'
#' @param x an object of class logMod.
#' @param ... methods are required to include (at least) the same arguments
#' as their generic functions.
#'
#' @export
plot.logMod <- function(x, ...) {


  # Plot 1 (Classification Matrix) ----

  # Predicted probability for success (read: if predicted probability greater
  # than 0.5 classify as 1 and 0 otherwise)
  y_hat <- ifelse(x$pi > 0.5, 1, 0)

  # Definie classification matrix
  classMat <- table(y_hat, x$model[ ,1])
  classMat

  # Probability of correct classification
  correctClass <- sum(diag(classMat))/sum(classMat)

  plot(classMat,
       main = "Classification Matrix",
       xlab = paste("Predicted Prob. vs. True Values"))




  # Plot 2 (Residuals vs Fitted) ----
  plot(x$pi, x$residuals,
       main = "Residuals vs Fitted",
       xlab = paste("Predicted Values\n", deparse(x$formula)),
       ylab = "Residuals")

  # Add horiztontal grey line to plot, which represents y = 0
  abline(a = 0, b = 0, lty = 3, col = "gray")

  # Add smooth curve
  lines(lowess(x = x$pi, y = x$residuals), col = "red")




  # Plot 2 (Distribution of Resduals) ----
  p <- qqnorm(x$residuals,
              main = "Normal Q-Q",
              xlab = paste("Theoretical Quantiles\n", deparse(x$formula)),
              ylab = "Std. deviance resid.")

  qqline(x$residuals, lty = 3, col = "gray")




  # Plot 4 (Scale Location) ----
  stdDevRes <- sqrt((x$residuals)^2)
  ylab <- as.expression(substitute(sqrt(abs(x)),
                                   list(x = as.name("Std. Deviance Resid."))))

  plot(x$pi, stdDevRes,
       main = "Scale Location",
       xlab = paste("Predicted Values\n", deparse(x$formula)),
       ylab = ylab)

  # Plot Smoothing via lowess regression
  lines(lowess(x = x$fittedValues,
               y = sqrt(abs(x$residuals))), col = "red")


}





