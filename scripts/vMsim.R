von_mises_loop <- function(n, kappa) {
  y <- numeric(n)
  for (i in 1:n) {
    reject <- TRUE
    while (reject) {
      y0 <- runif(1, - pi, pi)
      u <- runif(1)
      reject <- u > exp(kappa * (cos(y0) - 1))
    }
    y[i] <- y0
  }
  y
}