SG <- function(
  par, 
  grad,              # Function of parameter and observation index
  N,                 # Sample size
  gamma,             # Decay schedule or a fixed learning rate
  maxiter = 100,     # Max epoch iterations
  sampler = sample,  # How data is resampled. Default is a random permutation
  cb = NULL, 
  ...
) {
  gamma <- if (is.function(gamma)) gamma(1:maxiter) else rep(gamma, maxiter) 
  for(k in 1:maxiter) {
    if(!is.null(cb)) cb()
    samp <- sampler(N)   
    for(j in 1:N) {
      i <-  samp[j]
      par <- par - gamma[k] * grad(par, i, ...)
    }
  }
  par
}

ls_model <- function(X, y) {
  N <- length(y)
  X <- unname(X) # Strips X of names
  list(
    # Initial parameter value
    par0 = rep(0, ncol(X)),
    # Objective function
    H = function(beta) 
      drop(crossprod(y - X %*% beta)) / (2 * N),
    # Gradient in a single observation
    grad = function(beta, i) {  
      xi <- X[i, ]
      xi * drop(xi %*% beta - y[i])
    }
  )
}