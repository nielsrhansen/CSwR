stoch_grad <- function(
  par,
  grad,
  N,
  gamma,
  maxiter = 100,
  sampler = sample,
  cb = NULL,
  ...
) {
  gamma <- if (is.function(gamma)) gamma(1:maxiter) else rep(gamma, maxiter)
  for (k in 1:maxiter) {
    if (!is.null(cb)) {
      cb()
    }
    samp <- sampler(N)
    for (j in 1:N) {
      i <- samp[j]
      par <- par - gamma[k] * grad(par, i, ...)
    }
  }
  par
}

ls_model <- function(X, y) {
  N <- length(y)
  X <- unname(X)
  list(
    N = N,
    X = X,
    y = y,
    par0 = rep(0, ncol(X)),
    H = function(beta) {
      drop(crossprod(y - X %*% beta)) / (2 * N)
    },
    grad = function(beta, i, ...) {
      xi <- X[i, , drop = FALSE]
      res <- xi %*% beta - y[i]
      drop(crossprod(xi, res)) / length(res)
    }
  )
}

ls_model_old <- function(X, y) {
  N <- length(y)
  X <- unname(X) # Strips X of names
  list(
    N = N,
    y = y,
    par0 = rep(0, ncol(X)),
    H = function(beta) {
      drop(crossprod(y - X %*% beta)) / (2 * N)
    },
    grad = function(beta, i) {
      xi <- X[i, ]
      xi * drop(xi %*% beta - y[i])
    }
  )
}

