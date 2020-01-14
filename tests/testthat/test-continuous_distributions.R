test_that("d_normal works", {
  model_code <- 
    '
    functions {
      real d_normal_rng(real y, real mu, real sigma) {
        return normal_lpdf(y | mu, sigma);
      }
    }
'
  rstan::expose_stan_functions(rstan::stanc(model_code = model_code))
  
  expect_equal(
    d_normal(c(-10, 0, 10), 0, 1), 
    base::exp(c(
      d_normal_rng(-10, 0, 1),
      d_normal_rng(0, 0, 1),
      d_normal_rng(10, 0, 1)
    ))
  )
  
  expect_error(d_normal(0, 0, -1))
  expect_error(d_normal(0, 0, 0))
  
  expect_error(d_normal(NA, 0, 1))
  expect_error(d_normal(0, NA, 1))
  expect_error(d_normal(0, 0, NA))
  
  expect_error(d_normal("1", 0, 1))
  expect_error(d_normal(1, "0", 1))
  expect_error(d_normal(1, 0, "1"))
})
