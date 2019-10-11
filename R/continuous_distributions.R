#' Normal Distribution
#'
#' Description
#'
#' @param x A numeric vector of real numbers to distribute.
#' @param mu A single real number controlling the location of the distribution.
#' @param sigma A single positive real number controlling the spread of the
#'   distribution.
#' @return The probability density for each value of \code{x}.
#' @references
#' \url{https://mc-stan.org/docs/2_20/functions-reference/normal-distribution.html}
#' @family distributions
#' @family continuous distributions
#' @family unbounded continuous distributions
#' @seealso \code{\link[stats]{dnorm}} for the base R analog
#' @export
#' @examples
#' d_normal(x = -10:10, mu = 0, sigma = 1)
d_normal <- function(x, mu, sigma) {
  assert_that(is.numeric(x))
  assert_that(is.number(mu))
  assert_that(is.number(sigma))
  assert_that(sigma > 0)
  
  y <- (1 / (sqrt(2 * pi) * sigma)) * 
    base::exp(-0.5 * ((x - mu) / sigma)^2)
  y
}

#' Standard Normal Distribution
#'
#' Description
#'
#' @param x A numeric vector of real numbers to distribute.
#' @return The probability density for each value of \code{x}.
#' @references
#' \url{https://mc-stan.org/docs/2_20/functions-reference/normal-distribution.html}
#' @family distributions
#' @family continuous distributions
#' @family unbounded continuous distributions
#' @seealso \code{\link[stats]{dnorm}} for the base R analog
#' @export
#' @examples
#' d_std_normal(x = -10:10)
d_std_normal <- function(x) {
  assert_that(is.numeric(x))

  y <- (1 / (sqrt(2 * pi) * sigma)) * 
    base::exp(-x^2 / 2)
  y
}

# TODO: normal_id_glm ==========================================================

#' Exponentially Modified Normal Distribution
#'
#' Description
#'
#' @param x A numeric vector of real numbers to distribute.
#' @param mu A single real number that controls the location of the
#'   distribution.
#' @param sigma A single positive real number that controls the spread of the
#'   distribution.
#' @param lambda A single positive real number that controls the exponential
#'   modification of the distribution.
#' @return The probability density for each value of \code{x}.
#' @references
#' \url{https://mc-stan.org/docs/2_20/functions-reference/exponentially-modified-normal-distribution.html}
#' @family distributions
#' @family continuous distributions
#' @family unbounded continuous distributions
#' @export
#' @examples
#' d_exp_mod_normal(x = -10:10, mu = 0, sigma = 1, lambda = 1)
d_exp_mod_normal <- function(x, mu, sigma, lambda) {
  assert_that(is.numeric(x))
  assert_that(is.number(mu))
  assert_that(is.number(sigma))
  assert_that(is.number(lambda))
  assert_that(sigma > 0)
  assert_that(lambda > 0)
  
  y <- (lambda / 2) * 
    base::exp((lambda / 2) * (2 * mu + lambda * sigma^2 - 2 * x)) * 
    erfc((mu + lambda * sigma^2 - x) / (sqrt(2) * sigma))
  y
}

#' Skew Normal Distribution
#'
#' Description
#'
#' @param x A numeric vector of real numbers to distribute.
#' @param xi A single real number that controls the location of the
#'   distribution.
#' @param omega A single positive real number that controls the spread of the
#'   distribution.
#' @param alpha A single real number that controls the skew of the distribution.
#' @return The probability density for each value of \code{x}.
#' @references
#' \url{https://mc-stan.org/docs/2_20/functions-reference/skew-normal-distribution.html}
#' @family distributions
#' @family continuous distributions
#' @family unbounded continuous distributions
#' @export
#' @examples
#' d_skew_normal(x = -10:10, xi = 0, omega = 1, alpha = 0)
d_skew_normal <- function(x, xi, omega, alpha) {
  assert_that(is.numeric(x))
  assert_that(is.number(xi))
  assert_that(is.number(omega))
  assert_that(is.number(alpha))
  assert_that(omega > 0)
  
  y <- (1 / (omega * sqrt(2 * pi))) * 
    base::exp(-0.5 * ((x - xi) / (omega))^2) * 
    (1 + VGAM::erf(alpha * (x - xi) / (omega * sqrt(2))))
  y
}

#' Student-T Distribution
#'
#' Description
#'
#' @param x A numeric vector of real numbers to distribute.
#' @param nu A single positive real number that controls the degrees of freedom
#'   of the distribution.
#' @param mu A single real number that controls the location of the
#'   distribution.
#' @param sigma A single positive real number that controls the spread of the
#'   distribution.
#' @return The probability density for each value of \code{x}.
#' @references
#' \url{https://mc-stan.org/docs/2_20/functions-reference/student-t-distribution.html}
#' @family distributions
#' @family continuous distributions
#' @family unbounded continuous distributions
#' @seealso \code{\link[stats]{dt}} for the base R analog.
#' @export
#' @examples
#' d_student_t(x = -10:10, nu = 3, mu = 0, sigma = 1)
d_student_t <- function(x, nu, mu, sigma) {
  assert_that(is.numeric(x))
  assert_that(is.number(nu))
  assert_that(is.number(mu))
  assert_that(is.number(sigma))
  assert_that(nu > 0)
  assert_that(sigma > 0)
  
  y <- (gamma((nu + 1) / 2) / gamma(nu / 2)) * 
    (1 / (sqrt(nu * pi) * sigma)) * 
    ((1 + (1 / nu) * ((x - mu) / sigma)^2)^((nu + 1) / -2))
  y
}

#' Cauchy Distribution
#'
#' Description
#'
#' @param x A numeric vector of real numbers to distribute.
#' @param mu A single real number that controls the location of the
#'   distribution.
#' @param sigma A single positive real number that controls the spread of the
#'   distribution.
#' @return The probability density for each value of \code{x}.
#' @references
#' \url{https://mc-stan.org/docs/2_20/functions-reference/cauchy-distribution.html}
#' @family distributions
#' @family continuous distributions
#' @family unbounded continuous distributions
#' @export
#' @examples
#' d_cauchy(x = -10:10, mu = 0, sigma = 1)
d_cauchy <- function(x, mu, sigma) {
  assert_that(is.numeric(x))
  assert_that(is.number(mu))
  assert_that(is.number(sigma))
  assert_that(sigma > 0)
  
  y <- (1 / (pi * sigma)) * 
    (1 / (1 + ((x - mu) / sigma)^2))
  y
}

#' Double Exponential (Laplace) Distribution
#'
#' Description
#'
#' @param x A numeric vector of real numbers to distribute.
#' @param mu A single real number that controls the location of the
#'   distribution.
#' @param sigma A single positive real number that controls the spread of the
#'   distribution.
#' @return The probability density for each value of \code{x}.
#' @references
#' \url{https://mc-stan.org/docs/2_20/functions-reference/double-exponential-laplace-distribution.html}
#' @family distributions
#' @family continuous distributions
#' @family unbounded continuous distributions
#' @export
#' @examples
#' d_double_exponential(x = -10:10, mu = 0, sigma = 1)
d_double_exponential <- function(x, mu, sigma) {
  assert_that(is.numeric(x))
  assert_that(is.number(mu))
  assert_that(is.number(sigma))
  assert_that(sigma > 0)
  
  y <- (1 / (2 * sigma)) * 
    exp(-1 * abs(x - mu) / sigma)
  y
}

#' Logistic Distribution
#'
#' Description
#'
#' @param x A numeric vector of real numbers to distribute.
#' @param mu A single real number that controls the location of the
#'   distribution.
#' @param sigma A single positive real number that controls the spread of the
#'   distribution.
#' @return The probability density for each value of \code{x}.
#' @references
#' \url{https://mc-stan.org/docs/2_20/functions-reference/logistic-distribution.html}
#' @family distributions
#' @family continuous distributions
#' @family unbounded continuous distributions
#' @export
#' @examples
#' d_logistic(x = -10:10, mu = 0, sigma = 1)
d_logistic <- function(x, mu, sigma) {
  assert_that(is.numeric(x))
  assert_that(is.number(mu))
  assert_that(is.number(sigma))
  assert_that(sigma > 0)
  
  y <- (1 / sigma) * 
    base::exp(-1 * (x - mu) / sigma) * 
    (1 + base::exp(-1 * (x - mu) / sigma))^(-2)
  y
}

#' Gumbel Distribution
#'
#' Description
#'
#' @param x A numeric vector of real numbers to distribute.
#' @param mu A single real number that controls the location of the
#'   distribution.
#' @param beta A single positive real number that controls the spread of the
#'   distribution.
#' @return The probability density for each value of \code{x}.
#' @references
#' \url{https://mc-stan.org/docs/2_20/functions-reference/gumbel-distribution.html}
#' @family distributions
#' @family continuous distributions
#' @family unbounded continuous distributions
#' @export
#' @examples
#' d_gumbel(x = -10:10, mu = 0, beta = 1)
d_gumbel <- function(x, mu, beta) {
  assert_that(is.numeric(x))
  assert_that(is.number(mu))
  assert_that(is.number(beta))
  assert_that(beta > 0)
  
  y <- (1 / beta) * 
    base::exp(-((x - mu) / beta) - base::exp(-((x - mu) / beta)))
  y
}

#' Lognormal Distribution
#'
#' Description
#'
#' @param x A numeric vector of real numbers to distribute.
#' @param mu A single real number that controls the location of the
#'   distribution.
#' @param sigma A single positive real number that controls the spread of the
#'   distribution.
#' @return The probability density for each value of \code{x}.
#' @references
#' \url{https://mc-stan.org/docs/2_20/functions-reference/lognormal.html}
#' @family distributions
#' @family continuous distributions
#' @family positive continuous distributions
#' @export
#' @examples
#' d_lognormal(x = -10:10, mu = 0, sigma = 1)
d_lognormal <- function(x, mu, sigma) {
  assert_that(is.numeric(x))
  assert_that(is.number(mu))
  assert_that(is.number(sigma))
  assert_that(sigma > 0)
  
  # TODO: prevent warnings when x includes 0
  
  y <- (1 / (sqrt(2 * pi) * sigma)) * 
    (1 / x) * 
    base::exp(-0.5 * ((log(x) - mu) / sigma)^2)
  y[x <= 0] <- 0
  y
}

#' Chi-Square Distribution
#'
#' Description
#'
#' @param x A numeric vector of real numbers to distribute.
#' @param nu A single positive real number that controls the spread of the
#'   distribution.
#' @return The probability density for each value of \code{x}.
#' @references
#' \url{https://mc-stan.org/docs/2_20/functions-reference/chi-square-distribution.html}
#' @family distributions
#' @family continuous distributions
#' @family positive continuous distributions
#' @seealso \code{\link[stats]{dchisq}} for the base R analog.
#' @export
#' @examples
#' d_chi_square(x = -10:10, nu = 1)
d_chi_square <- function(x, nu) {
  assert_that(is.numeric(x))
  assert_that(is.number(nu))
  assert_that(nu > 0)
  
  y <- 2^(nu / -2) / base::gamma(nu / 2) * 
    x^(nu / 2 - 1) * 
    base::exp(-0.5 * x)
  y[x <= 0] <- 0
  y
}

#' Inverse Chi-Square Distribution
#'
#' Description
#'
#' @param x A numeric vector of real numbers to distribute.
#' @param nu A single positive real number that controls the height of the
#'   distribution.
#' @return The probability density for each value of \code{x}.
#' @references
#' \url{https://mc-stan.org/docs/2_20/functions-reference/inverse-chi-square-distribution.html}
#' @family distributions
#' @family continuous distributions
#' @family positive continuous distributions
#' @export
#' @examples
#' d_inv_chi_square(x = -10:10, nu = 1)
d_inv_chi_square <- function(x, nu) {
  assert_that(is.numeric(x))
  assert_that(is.number(nu))
  assert_that(nu > 0)
  
  y <- 2^(-nu / 2) / base::gamma(nu / 2) * 
    x^(-nu / 2 - 1) * 
    base::exp(-0.5 * 1 / x)
  y[x <= 0] <- 0
  y
}

#' Scaled Inverse Chi-Square Distribution
#'
#' Description
#'
#' @param x A numeric vector of real numbers to distribute.
#' @param nu A single positive real number that controls the height of the
#'   distribution.
#' @param sigma A single positive real number that controls the spread of the
#'   distribution.
#' @return The probability density for each value of \code{x}.
#' @references
#' \url{https://mc-stan.org/docs/2_20/functions-reference/scaled-inverse-chi-square-distribution.html}
#' @family distributions
#' @family continuous distributions
#' @family positive continuous distributions
#' @export
#' @examples
#' d_scaled_inv_chi_square(x = -10:10, nu = 1, sigma = 1)
d_scaled_inv_chi_square <- function(x, nu, sigma) {
  assert_that(is.numeric(x))
  assert_that(is.number(nu))
  assert_that(is.number(sigma))
  assert_that(nu > 0)
  assert_that(sigma > 0)
  
  y <- (nu / 2) ^ (nu / 2) / base::gamma(nu / 2) * 
    sigma^nu * 
    x^(-(nu / 2 + 1)) * 
    base::exp(-0.5 * nu * sigma^2 * (1 / x))
  y[x <= 0] <- 0
  y
}

#' Exponential Distribution
#'
#' Description
#'
#' @param x A numeric vector of real numbers to distribute.
#' @param beta A single positive real number that controls the peakedness of the
#'   distribution.
#' @return The probability density for each value of \code{x}.
#' @references
#' \url{https://mc-stan.org/docs/2_20/functions-reference/exponential-distribution.html}
#' @family distributions
#' @family continuous distributions
#' @family positive continuous distributions
#' @export
#' @examples
#' d_exponential(x = -10:10, beta = 1)
d_exponential <- function(x, beta) {
  assert_that(is.numeric(x))
  assert_that(is.number(beta))
  assert_that(beta > 0)
  
  y <- beta * 
    base::exp(-beta * x)
  y[x <= 0] <- 0
  y
}

#' Gamma Distribution
#'
#' Description
#'
#' @param x A numeric vector of real numbers to distribute.
#' @param alpha A single positive real number that controls the spread of the
#'   distribution.
#' @param beta A single positive real number that controls the peakedness of the
#'   distribution.
#' @return The probability density for each value of \code{x}.
#' @references
#' \url{https://mc-stan.org/docs/2_20/functions-reference/gamma-distribution.html}
#' @family distributions
#' @family continuous distributions
#' @family positive continuous distributions
#' @export
#' @examples
#' d_gamma(x = -10:10, alpha = 1, beta = 1)
d_gamma <- function(x, alpha, beta) {
  assert_that(is.numeric(x))
  assert_that(is.number(alpha))
  assert_that(is.number(beta))
  assert_that(alpha > 0)
  assert_that(beta > 0)
  
  y <- beta^alpha / base::gamma(alpha) * 
    x^(alpha - 1) * 
    base::exp(-beta * x)
  y[x <= 0] <- 0
  y
}

#' Inverse Gamma Distribution
#'
#' Description
#'
#' @param x A numeric vector of real numbers to distribute.
#' @param alpha A single positive real number that controls the spread of the
#'   distribution.
#' @param beta A single positive real number that controls the peakedness of the
#'   distribution.
#' @return The probability density for each value of \code{x}.
#' @references
#' \url{https://mc-stan.org/docs/2_20/functions-reference/inverse-gamma-distribution.html}
#' @family distributions
#' @family continuous distributions
#' @family positive continuous distributions
#' @export
#' @examples
#' d_inv_gamma(x = -10:10, alpha = 1, beta = 1)
d_inv_gamma <- function(x, alpha, beta) {
  assert_that(is.numeric(x))
  assert_that(is.number(alpha))
  assert_that(is.number(beta))
  assert_that(alpha > 0)
  assert_that(beta > 0)
  
  y <- beta^alpha / base::gamma(alpha) * 
    x^(-1 * (alpha + 1)) * 
    base::exp(-beta * 1 / x)
  y[x <= 0] <- 0
  y
}

#' Weibull Distribution
#'
#' Description
#'
#' @param x A numeric vector of real numbers to distribute.
#' @param alpha A single positive real number that controls the ... of the
#'   distribution.
#' @param sigma A single positive real number that controls the spread of the
#'   distribution.
#' @return The probability density for each value of \code{x}.
#' @references
#' \url{https://mc-stan.org/docs/2_20/functions-reference/weibull-distribution.html}
#' @family distributions
#' @family continuous distributions
#' @family positive continuous distributions
#' @export
#' @examples
#' d_weibull(x = -10:10, alpha = 1, sigma = 1)
d_weibull <- function(x, alpha, sigma) {
  assert_that(is.numeric(x))
  assert_that(is.number(alpha))
  assert_that(is.number(sigma))
  assert_that(alpha > 0)
  assert_that(sigma > 0)
  
  y <- (alpha / sigma) * 
    (x / sigma)^(alpha - 1) * 
    base::exp(-(x / sigma)^alpha)
  y[x < 0] <- 0
  y
}

#' Frechet Distribution
#'
#' Description
#'
#' @param x A numeric vector of real numbers to distribute.
#' @param alpha A single positive real number that controls the ... of the
#'   distribution.
#' @param sigma A single positive real number that controls the spread of the
#'   distribution.
#' @return The probability density for each value of \code{x}.
#' @references
#' \url{https://mc-stan.org/docs/2_20/functions-reference/frechet-distribution.html}
#' @family distributions
#' @family continuous distributions
#' @family positive continuous distributions
#' @export
#' @examples
#' d_frechet(x = -10:10, alpha = 1, sigma = 1)
d_frechet <- function(x, alpha, sigma) {
  assert_that(is.numeric(x))
  assert_that(is.number(alpha))
  assert_that(is.number(sigma))
  assert_that(alpha > 0)
  assert_that(sigma > 0)
  
  y <- (alpha / sigma) * 
    (x / sigma)^(-alpha - 1) * 
    base::exp(-(x / sigma)^(-alpha))
  y[x < 0] <- 0
  y
}

#' Rayleigh Distribution
#'
#' Description
#'
#' @param x A numeric vector of real numbers to distribute.
#' @param sigma A single positive real number that controls the spread of the
#'   distribution.
#' @return The probability density for each value of \code{x}.
#' @references
#' \url{https://mc-stan.org/docs/2_20/functions-reference/rayleigh-distribution.html}
#' @family distributions
#' @family continuous distributions
#' @family non-negative continuous distributions
#' @export
#' @examples 
#' d_rayleigh(x = -10:10, sigma = 1)
d_rayleigh <- function(x, sigma) {
  assert_that(is.numeric(x))
  assert_that(is.number(sigma))
  assert_that(sigma > 0)
  
  y <- (x / sigma^2) * 
    base::exp(-(x^2) / (2 * sigma^2))
  y[x < 0] <- 0
  y
}

# TODO: wiener

#' Pareto Distribution
#'
#' Description
#'
#' @param x A numeric vector of real numbers to distribute.
#' @param xmin A single positive real number that controls the lower bound of
#'   the distribution.
#' @param alpha A single positive real number that controls the spread of the
#'   distribution.
#' @return The probability density for each value of \code{x}.
#' @references
#' /url{https://mc-stan.org/docs/2_20/functions-reference/pareto-distribution.html}
#' @family distributions
#' @family continuous distributions
#' @family bounded continuous distributions
#' @export
#' @examples
#' d_pareto(x = -10:10, xmin = 2, alpha = 1)
d_pareto <- function(x, xmin, alpha) {
  assert_that(is.numeric(x))
  assert_that(is.number(xmin))
  assert_that(is.number(alpha))
  assert_that(xmin > 0)
  assert_that(alpha > 0)
  
  y <- (alpha * xmin^alpha) / (x^(alpha + 1))
  y[x < xmin] <- 0
  y
}

#' Pareto Type 2 Distribution
#'
#' Description. Note that the Lomax distribution is a Pareto Type 2 distribution
#' with \code{mu=0}.
#'
#' @param x A numeric vector of real numbers to distribute.
#' @param mu A single real number that controls the location of the
#'   distribution.
#' @param lambda A single positive real number that controls the lower bound of
#'   the distribution.
#' @param alpha A single positive real number that controls the spread of the
#'   distribution.
#' @return The probability density for each value of \code{x}.
#' @references
#' /url{https://mc-stan.org/docs/2_20/functions-reference/pareto-type-2-distribution.html}
#' @family distributions
#' @family continuous distributions
#' @family bounded continuous distributions
#' @export
#' @examples
#' d_pareto_type_2(x = -10:10, mu = 0, lambda = 1, alpha = 1)
d_pareto_type_2 <- function(x, mu, lambda, alpha) {
  assert_that(is.numeric(x))
  assert_that(is.number(mu))
  assert_that(is.number(lambda))
  assert_that(is.number(alpha))
  assert_that(lambda > 0)
  assert_that(alpha > 0)
  
  # TODO: Check that this is working properly
  
  y <- (alpha / lambda) * 
    (1 + (x - mu) / lambda)^(-(alpha + 1))
  y[x < mu] <- 0
}

#' Beta Distribution
#'
#' Description.
#'
#' @param x A numeric vector of real numbers to distribute.
#' @param alpha A single positive real number that controls how much the
#'   distribution is pushed towards 1.
#' @param beta A single positive real number that controls how much the
#'   distribution is pushed towards 0.
#' @return The probability density for each value of \code{x}.
#' @references
#' /url{https://mc-stan.org/docs/2_20/functions-reference/beta-distribution.html}
#' @family distributions
#' @family continuous distributions
#' @family bounded continuous distributions
#' @seealso \code{\link[stats]{dbeta}} for the base R analog.
#' @export
#' @examples
#' d_beta(x = -1:2, alpha = 1, beta = 1)
d_beta <- function(x, alpha, beta) {
  assert_that(is.numeric(x))
  assert_that(is.number(alpha))
  assert_that(is.number(beta))
  assert_that(alpha > 0)
  assert_that(beta > 0)
  
  y <- (1 / base::beta(alpha, beta)) * 
    x^(alpha - 1) * 
    (1 - x)^(beta - 1)
  y[x <= 0 | x >= 1] <- 0
  y
}

#' Beta Proportion Distribution
#'
#' Description.
#'
#' @param x A numeric vector of real numbers to distribute.
#' @param mu A single real number between 0 and 1 (but not including 0 or 1)
#'   that controls the location of the distribution.
#' @param kappa A single positive real number that controls the shape of the
#'   distribution.
#' @return The probability density for each value of \code{x}.
#' @references
#' /url{https://mc-stan.org/docs/2_20/functions-reference/beta-proportion-distribution.html}
#' @family distributions
#' @family continuous distributions
#' @family bounded continuous distributions
#' @export
#' @examples
#' d_beta_proportion(x = -1:2, mu = 0.5, kappa = 1)
d_beta_proportion <- function(x, mu, kappa) {
  assert_that(is.numeric(x))
  assert_that(is.number(mu))
  assert_that(is.number(kappa))
  assert_that(mu > 0 & mu < 1)
  assert_that(kappa > 0)
  
  y <- (1 / base::beta(mu * kappa, (1 - mu) * kappa)) * 
    x^(mu * kappa - 1) * 
    (1 - x)^(kappa * (1 - mu) - 1)
  y[x <= 0 | x >= 1] <- 0
  y
}

# TODO: von_mises

#' Uniform Distribution
#'
#' Description.
#'
#' @param x A numeric vector of real numbers to distribute.
#' @param alpha A single real number that controls the minimum value of the
#'   distribution.
#' @param beta A single real number that controls the maximum value of the
#'   distribution.
#' @return The probability density for each value of \code{x}.
#' @references
#' /url{https://mc-stan.org/docs/2_20/functions-reference/uniform-distribution.html}
#' @family distributions
#' @family continuous distributions
#' @family bounded continuous distributions
#' @seealso \code{\link[stats]{dunif}} for the base R analog.
#' @export
#' @examples
#' d_uniform(x = -10:10, alpha = -5, beta = 5)
d_uniform <- function(x, alpha, beta) {
  assert_that(is.numeric(x))
  assert_that(is.number(alpha))
  assert_that(is.number(beta))
  assert_that(beta > alpha)
  
  y <- 1 / (beta - alpha)
  y[x < alpha | x > beta] <- 0
  y
}
