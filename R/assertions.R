# Check if input is a natural number
is_natural_number <- function(x) {
  is.numeric(x) && length(x) == 1 && x >= 0 && floor(x) == ceiling(x)
}

assertthat::on_failure(is_natural_number) <- function(call, env) {
  paste0(deparse(call$x), " is not a natural number (a single zero or positive integer).")
}

# Check if input is a vector of natural numbers
is_natural_vector <- function(x) {
  is.numeric(x) && all(x >= 0) && all(x == trunc(x))
}

assertthat::on_failure(is_natural_vector) <- function(call, env) {
  paste0(deparse(call$x), " is not a vector of natural numbers (zeroes or positive integers).")
}
