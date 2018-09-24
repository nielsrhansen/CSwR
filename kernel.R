kernDens <- function (x, h, m = 512) {
  rg <- range(x)
  ## xx is equivalent to grid points in 'density'
  xx <- seq(rg[1] - 3 * h, rg[2] + 3 * h, length.out = m)
  y <- numeric(m) ## The evaluations, initialized as a vector of zeroes
  ## The actual computation is done using nested for-loops. The outer loop
  ## is over the grid points, and the inner loop is over the data points.
  for (i in seq_along(xx)) {
    for (j in seq_along(x)) {
      y[i] <- y[i] + exp(- (xx[i] - x[j])^2 / (2 * h^2))  
    }
  }
  y <- y / (sqrt(2 * pi) * h * length(x))
  list(x = xx, y = y)
}

kernDens_prof <- function (x, h, m = 512) {
  rg <- range(x)
  ## xx is equivalent to grid points in 'density'
  xx <- seq(rg[1] - 3 * h, rg[2] + 3 * h, length.out = m)
  y <- numeric(m) ## The evaluations, initialized as a vector of zeroes
  ## The actual computation is done using nested for-loops. The outer loop
  ## is over the grid points, and the inner loop is over the data points.
  for (i in seq_along(xx)) {
    for (j in seq_along(x)) {
      ## y[i] <- y[i] + exp(- (xx[i] - x[j])^2 / (2 * h^2))  
      z <- xx[i] - x[j]
      z <- z^2
      z <- z / (2 * h^2)
      z <- exp(-z)
      y[i] <- y[i] + z
    }
  }
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
    y[i] <- sum(exp(-(xx[i] - x)^2 / (2 * h^2)) / (sqrt(2 * pi) * h * length(x)))
  list(x = xx, y = y)
}

kernDens_apply <- function (x, h, m = 512) {
  rg <- range(x)
  xx <- seq(rg[1] - 3 * h, rg[2] + 3 * h, length.out = m)
  y <- sapply(xx, function(z) sum(exp(-(z - x)^2 / (2 * h^2)) / (sqrt(2 * pi) * h * length(x))))
  list(x = xx, y = y)
}

kernDens_outer <- function (x, h, m = 512) {
  rg <- range(x)
  xx <- seq(rg[1] - 3 * h, rg[2] + 3 * h, length.out = m)
  y <- outer(xx, x, function(zz, z) exp(-(zz - z)^2 / (2 * h^2)))
  y <- rowMeans(y) / (sqrt(2 * pi) * h)
  list(x = xx, y = y)
}

kernbin <- function(x, lo, hi, m) {
  w <- numeric(m)
  delta <- (hi - lo) / (m - 1)
  for(i in seq_along(x)) {
    ii <- floor((x[i] - lo) / delta + 0.5) + 1
    w[ii] <- w[ii] + 1
  }
  w / sum(w)
}

## This implementation assumes a symmetric kernel! 
## It's possible to avoid the symmetry assumption, 
## but it's a little more complicated.
kernDens_bin_toep <- function(x, h, m = 512) {
  rg <- range(x) + c(- 3* h, 3 * h)
  xx <- seq(rg[1], rg[2], length.out = m)
  weights <- kernbin(x, rg[1], rg[2], m)
  kerneval <- exp(- (xx - xx[1])^2 / (2 * h^2)) / (sqrt(2 * pi) * h)
  kerndif <- matrix(kerneval[toeplitz(1:m)], m, m)
  y <- colSums(weights * kerndif)
  list(x = xx, y = y, h = h)
}

kernDens_bin <- function(x, h, m = 512) {
  rg <- range(x) + c(- 3* h, 3 * h)
  xx <- seq(rg[1], rg[2], length.out = m)
  weights <- kernbin(x, rg[1], rg[2], m)
  kerneval <- exp(- (xx - xx[1])^2 / (2 * h^2)) / (sqrt(2 * pi) * h)
  kerndif <- toeplitz(kerneval)
  y <- colSums(weights * kerndif)
  list(x = xx, y = y, h = h)
}

kernDens_bin_conv <- function(x, h, m = 512) {
    rg <- range(x) + c(- 3* h, 3 * h)
    xx <- seq(rg[1], rg[2], length.out = m)
    weights <- kernbin(x, rg[1], rg[2], m)
    kerneval <- exp(- (xx - xx[1])^2 / (2 * h^2)) / (sqrt(2 * pi) * h)
    browser()
    y <- fft(fft(weights) * Conj(fft(c(kerneval, rev(kerneval)))), inverse = TRUE)
    list(x = xx, y = y, h = h)
  }














