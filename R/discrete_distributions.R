#' Bernoulli Distribution
#'
#' Description
#'
#' @param x A numeric vector of 0s and 1s to distribute.
#' @param theta A single real number between 0 and 1 (inclusive) that controls
#'   the spread of the distribution.
#' @return The probability density for each value of \code{x}.
#' @references
#' \url{https://mc-stan.org/docs/2_20/functions-reference/bernoulli-distribution.html}
#' @family distributions
#' @family discrete distributions
#' @family binary distributions
#' @seealso \code{\link[stats]{dnorm}} for the base R analog
#' @export
#' @examples
#' d_bernoulli(x = -10:10, mu = 0, sigma = 1)
d_bernoulli <- function(x, theta) {
  assert_that(is.numeric(x))
  assert_that(is.number(theta))
  assert_that(theta >= 0 & theta <= 1)
  
  # TODO: Figure out how to plot this
  
  y <- rep(NA, length(x))
  y[x == 1] <- theta
  y[x == 0] <- 1 - theta
  y
}

# Bernoulli distribution, logit parameterization
d_bernoulli_logit <- function(x, alpha) {
  assert_that(x %in% c(0, 1))
  assert_that(is.number(alpha))
  
  y <- rep(NA, length(x))
  y[x == 1] <- inv_logit(alpha)
  y[x == 1] <- 1 - inv_logit(alpha)
  y
}

# Binomial distribution
# dbinom: N = size, theta = prob
d_binomial <- function(x, N, theta) {
  assert_that(is_natural_vector(x)) # should non-natural numbers be 0 or errors?
  assert_that(is_natural_number(N))
  assert_that(is.number(theta))
  assert_that(theta >= 0 & theta <= 1)
  
  y <- base::choose(N, x) * 
    theta^x * 
    (1 - theta)^(N - x)
  y[x > N] <- 0
  y
}

# Binomial distribution, logit parameterization
d_binomial_logit <- function(x, N, alpha) {
  assert_that(is_natural_vector(x)) # should non-natural numbers be 0 or errors?
  assert_that(is_natural_number(N))
  assert_that(is.number(alpha))
  
  y <- base::choose(N, x) * 
    inv_logit(alpha)^x * 
    (1 - inv_logit(alpha))^(N - x)
  y[x > N] <- 0
  y
}

# Beta-binomial distribution
d_beta_binomial <- function(x, N, alpha, beta) {
  assert_that(is_natural_vector(x)) # should non-natural numbers be 0 or errors?
  assert_that(is_natural_number(N))
  assert_that(is.count(alpha))
  assert_that(is.count(beta))
  
  y <- base::choose(N, x) * 
    base::beta(x + alpha, N - x + beta) / 
    base::beta(alpha, beta)
  y[x > N] <- 0
  y
}

# Hypergeometric distribution
# dhyper: N = k, a = m, b = n
d_hypergeometric <- function(x, N, a, b) {
  assert_that(is_natural_vector(x)) # should non-natural numbers be 0 or errors?
  assert_that(is_natural_number(N))
  assert_that(is_natural_number(a))
  assert_that(is_natural_number(b))
  assert_that(N <= a + b)
  
  y <- (base::choose(a, x) * base::choose(b, N - x)) / (base::choose(a + b, N))
  y[x < max(0, N - b) | x > min(a, N)] <- 0
  y
}

# Negative binomial distribution
d_neg_binomial <- function(x, alpha, beta) {
  assert_that(is.numeric(x))
  assert_that(is.number(alpha))
  assert_that(is.number(beta))
  assert_that(alpha > 0)
  assert_that(beta > 0)
  # x is a natural number
  
  y <- base::choose(x + alpha - 1, alpha - 1) *
    (beta / (beta + 1))^alpha *
    (1 / (beta + 1))^x
  y
}

# Negative binomial distribution, alternative parameterization
d_neg_binomial_2 <- function(x, mu, phi) {
  assert_that(is.numeric(x))
  assert_that(is.number(mu))
  assert_that(is.number(phi))
  assert_that(mu > 0)
  assert_that(phi > 0)
  # x is a natural number
  
  y <- base::choose(x + phi - 1, x) *
    (mu / (mu + phi))^x *
    (phi / (mu + phi))^phi
  y
}

# Negative binomial distribution, log alternative parameterization
d_neg_binomial_2_log <- function(x, eta, phi) {
  assert_that(is.numeric(x))
  assert_that(is.number(eta))
  assert_that(is.number(phi))
  assert_that(phi > 0)
  # x is a natural number
  
  y <- d_neg_binomial_2(x, exp(eta), phi)
  y
}

# Poisson distribution
d_poisson <- function(x, lambda) {
  assert_that(is.numeric(x))
  assert_that(is.number(lambda))
  assert_that(lambda > 0)
  # x is a natural number
  
  y <- (1 / base::factorial(x)) *
    lambda^x *
    base::exp(-lambda)
  y
}

# Poisson distribution, log parameterization
d_poisson_log <- function(x, alpha) {
  assert_that(is.numeric(x))
  assert_that(is.number(alpha))
  # x is a natural number
  
  y <- (1 / base::factorial(x)) *
    base::exp(x * alpha - base::exp(alpha))
  y
}
