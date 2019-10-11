#' Visualize one or more specified distributions
#'
#' Description
#'
#' @param ... One or more distribution specification strings.
#' @param xlim A two-element numerical vector containing the minimum and maximum
#'   values to calculate and visualize the probability density across.
#' @return A ggplot object depicting each distribution in `...` between the
#'   minimum and maximum x-values defined in `xlim`.
#' @family visualiztion
#' @export
visualize <- function(..., xlim = c(-10, 10)) {
  
  # Check the xlim argument
  assert_that(is.numeric(xlim), 
              msg = "'xlim' must be numeric")
  assert_that(length(xlim) == 2, 
              msg = "'xlim' must contain two numbers: a minimum and a maximum")
  assert_that(xlim[[2]] > xlim[[1]], 
              msg = "The second number in 'xlim' must be larger than the first")
  
  # Initialize ggplot object 
  p <- ggplot2::ggplot(data.frame(x = xlim), ggplot2::aes(x)) +
    ggplot2::labs(color = "Distribution") + 
    ggplot2::theme(legend.position = "top")
  
  # Capture all distributions in a list
  ds <- list(...)
  
  # Loop through each distribution in the list
  for (i in seq_along(ds)) {
    
    # Get current distribution
    d_i <- ds[[i]]
    
    # Check that current distribution is a nonempty string
    assert_that(is.string(d_i), 
                msg = "Distributions must be strings")
    assert_that(nzchar(d_i), 
                msg = "Distributions cannot be empty strings")
    
    # Convert current distribution string into its function and argument components
    str_i <- stringr::str_remove_all(d_i, "[\\s\\)]")
    str_i <- stringr::str_split(str_i, "[\\(\\,]", simplify = TRUE)
    fun_i_raw <- str_i[[1]]
    fun_i_str <- paste0("d_", fun_i_raw)
    args_i <- as.list(as.double(str_i[-1]))
    
    # Check that a function exists for the current distribution
    assert_that(exists(fun_i_str, mode = "function") == TRUE,
                msg = "Could not find function for specified distribution")
    
    # Convert the current distribution from a string into a function
    fun_i <- base::eval(base::parse(text = fun_i_str))
    
    # Check that the number of arguments in current distribution matches the 
    # number of formals for its function
    formals_i <- base::formals(fun_i)
    assert_that(length(args_i) == length(formals_i) - 1,
                msg = "The distribution had too many or too few parameters")
    
    # Add a plot of the function to the ggplot object
    p <- p + ggplot2::stat_function(
      ggplot2::aes(color = !!{{d_i}}), 
      size = 1.5,
      fun = fun_i, 
      args = args_i
    )
  }
  
  # Return the complete ggplot object
  p
}
