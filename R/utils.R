#' Inverse Logit Function
#' 
#' Computes the inverse logit function (also called the logistic function).
#' 
#' @param x A numeric vector.
#' @return The inverse logit function for \code{x}.
#' @export
inv_logit <- function(x) {
  exp(x) / (exp(x) + 1)
}

#' Error Function 
#' 
#' Computes the error function based on the normal distribution.
#' 
#' @param x A numeric vector.
#' @return The error function for \code{x}.
#' @seealso \code{\link[VGAM]{erf}}
#' @export
erf <- function(x) {
  2 * stats::pnorm(x * sqrt(2)) - 1
}

#' Error Function Complement
#' 
#' Computes the complement of the error function based on the normal distribution.
#' 
#' @param x A numeric vector.
#' @return The complement of the error function for \code{x}.
#' @seealso \code{\link[VGAM]{erfc}}
#' @export
erfc <- function(x) {
  2 * stats::pnorm(sqrt(2), lower.tail = FALSE)
}