################################################################################
# 2.1 Computing the Log-Likelihood of the Null Model
################################################################################

# The function logLikeNull computes the logLikelihood of the Null Model.
# The ouput of the function is needed to calculate the Null Deviance in
# subsequent steps. ----
logLikeNull <- function(x) {

  # Create the design matrix for the null model consisting out of one vector
  # with entries of value 1 only
  xNull <-  as.matrix(rep(1, length(x)))

  # Apply the fishScore() function to compute the coeefficients for this model
  restrMod <- fishScore(xNull, x)

  # Store the computed coefficients in a new object
  betaNull <- restrMod$coefficients

  # Log-Likelihood computation
  logLikelihoodNull <- (sum((x * xNull %*% betaNull) -
                              (log(1 + exp(xNull %*% betaNull)))))

  return(logLikelihoodNull)
}

