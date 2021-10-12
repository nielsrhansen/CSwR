
grad_models <- function(X, y) {
  N <- length(y)
  tX <- t(X)     # To slice-by-columns (due to column major order)
  
  list(
    # Gradient in a single observation
    grad1 = function(beta, i) {               
      xi <- X[i, ]
      xi * drop(xi %*% beta - y[i])
    },
    # Gradient in a single observation
    grad1t = function(beta, i) {               
      xi <- tX[, i]
      xi * drop(beta %*% xi - y[i])
    },
    # Gradient 
    grad = function(beta, i) {
      xi <- X[i, , drop = FALSE]
      res <- xi %*% beta - y[i]
      drop(crossprod(xi, res)) / length(i)
    },
    # Gradient 
    gradt = function(beta, i) {
      xi <- tX[, i, drop = FALSE]
      res <- drop(beta %*% xi - y[i])
      drop(tcrossprod(res, xi)) / length(i)
    }
  )
}
