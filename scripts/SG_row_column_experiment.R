library(profvis)
library(CSwR)

stoch_grad <- function(
  par,
  N,                # Sample size
  gamma,            # Decay schedule or a fixed learning rate
  epoch = batch,    # Epoch update function
  ...,              # Other arguments passed to epoch updates
  maxit = 100,      # Max epoch iterations
  sampler = sample, # Data resampler. Default is random permutation
  cb = NULL
) {
  if (is.function(gamma)) {
    gamma <- gamma(1:maxit)
  }
  gamma <- rep_len(gamma, maxit)
  for (n in 1:maxit) {
    if (!is.null(cb)) {
      cb()
    }
    samp <- sampler(N)
    par <- epoch(par, samp, gamma[n], ...)
  }
  par
}

batch <- function(
  par,
  samp,
  gamma,
  grad,   # Function of parameter and observation index
  m = 50, # Mini-batch size
  ...
) {
  M <- floor(length(samp) / m)
  for (j in 0:(M - 1)) {
    i <- samp[(j * m + 1):(j * m + m)]
    par <- par - gamma * grad(par, i, ...)
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

ls_model_transpose <- function(X, y) {
  N <- length(y)
  X <- t(unname(X)) # Strips X of names
  list(
    N = N,
    X = X,
    y = y,
    par0 = rep(0, nrow(X)),
    H = function(beta) {
      drop(crossprod(drop(y - beta %*% X))) / (2 * N)
    },
    grad = function(beta, i, ...) {
      xi <- X[, i, drop = FALSE]
      res <- drop(beta %*% xi - y[i])
      drop(xi %*% res) / length(res)
    }
  )
}
