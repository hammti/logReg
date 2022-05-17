#' The Titanic Data Set
#'
#' A dataset containing information on the passengers of the Titanic. The
#' variable of interest is whether a passenger survived or not. Additionally,
#' there are two further variables indicating the passenger class and the
#' gender of a passenger. The data set is a reduced version of the original
#' Titanic data set.
#'
#'
#' @format A data frame with 891 rows and 3 variables:
#' \describe{
#'   \item{Survived}{int. 1 = Survived, 0 = Died}
#'   \item{Pclass}{int. 1 = Class 1, 2 = Class 2, 3 = Class 3}
#'   \item{Sex}{chr. "male", "female"}
#'
#' }
#' @source \url{https://github.com/paulhendricks/titanic}
#' @examples
#' data(titanicData)
"titanicData"
