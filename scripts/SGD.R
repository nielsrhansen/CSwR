SGD <- function(
  par,
  grad_obs,
  N, # Sample size
  gamma = 1e-5, # Learning rate
  maxiter = 40, # Max epoch iterations
  cb = NULL,
  ...
) {
  for (k in 1:maxiter) {
    samp <- sample(N, replace = TRUE) # Bootstrap sampling
    for (j in 1:N) {
      i <- samp[j]
      par <- par - gamma * grad_obs(par, i, ...)
    }
    if (!is.null(cb)) cb()
  }
  par
}

grad_ls <- function(X, y) {
  N <- nrow(X)
  X <- t(X)
  force(y)

  list(
    # Objective function
    function(beta) {
      drop(crossprod(y - beta %*% X)) / (2 * N)
    },

    # Gradient in a single observation
    function(beta, i) {
      xi <- X[, i]
      res <- drop(beta %*% xi - y[i])
      xi * res
    }
  )
}
