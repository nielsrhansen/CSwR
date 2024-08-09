gauss <- function(x, h = 1) {
  exp(- x^2 / (2 * h^2)) / (h * sqrt(2 * pi))
}

gauss_step <- function(x, h = 1) {
  exponent <- x^2 / (2 * h^2) 
  exp(- exponent) / (h * sqrt(2 * pi))
}

gauss_step_alt <- function(x, h = 1) {
  exponent <- (x / h)^2 / 2
  exp(- exponent) / (h * sqrt(2 * pi))
}