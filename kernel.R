kernDens <- function (x, h, m = 512) {
  rg <- range(x)
  ## xx is equivalent to grid points in 'density'
  xx <- seq(rg[1] - 3 * h, rg[2] + 3 * h, length.out = m)
  y <- numeric(m) ## The evaluations, initialized as a vector of zeroes
  ## The actual computation is done using nested for-loops. The outer loop
  ## is over the grid points, and the inner loop is over the data points.
  for (i in seq_along(xx))
    for (j in seq_along(x))
      y[i] <- y[i] + exp(- (xx[i] - x[j])^2 / (2 * h^2))  
  y <- y / (sqrt(2 * pi) * h * length(x))
  list(x = xx, y = y)
}

kernDens_vec <- function (x, h, m = 512) {
  rg <- range(x)
  xx <- seq(rg[1] - 3 * h, rg[2] + 3 * h, length.out = m)
  y <- numeric(m) 
  ## The inner loop from 'kernDens' has been vectorized, and only the 
  ## outer loop over the grid points remains.
  for (i in seq_along(xx))
    y[i] <- sum(exp(-(xx[i] - x)^2 / (2 * h^2)) / (sqrt(2 * pi) * h *length(x)))
  list(x = xx, y = y)
}

kernDens_apply <- function (x, h, m = 512) {
  rg <- range(x)
  xx <- seq(rg[1] - 3 * h, rg[2] + 3 * h, length.out = m)
  y <- sapply(xx, function(z) sum(exp(-(z - x)^2 / (2 * h^2)) / (sqrt(2 * pi) * h *length(x))))
  list(x = xx, y = y)
}

kernDens_outer <- function (x, h, m = 512) {
  rg <- range(x)
  xx <- seq(rg[1] - 3 * h, rg[2] + 3 * h, length.out = m)
  y <- outer(xx, x, function(zz, z) exp(-(zz - z)^2 / (2 * h^2)))
  y <- rowMeans(y) / (sqrt(2 * pi) * h)
  list(x = xx, y = y)
}