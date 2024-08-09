#' Random number stream based on caching
#'
#' A random number stream uses a vectorized random number generator to generate
#' a cache of random numbers that can then be used sequentially. Whenever the
#' cache runs empty new numbers are generated automatically.
#'
#' @param m initial cache size
#' @param rng a random number generator
#' @param ... additional arguments passed to the random number generator
#'
#' @return A function that extracts random numbers from the cache and
#' fills the cache whenever it runs empty.
#' @export
#'
#' @examples
#'
#' runif_stream <- rng_stream(10, runif, min = -1, max = 1)
#' runif_stream()
rng_stream <- function(m, rng, ...) {
  args <- list(...)
  cache <- do.call(rng, c(m, args))
  j <- 0
  fact <- 1
  next_rn <- function(r = m) {
    j <<- j + 1
    if (j > m) {
      if (fact == 1 && r < m) fact <<- m / (m - r)
      m <<- floor(fact * (r + 1))
      cache <<- do.call(rng, c(m, args))
      j <<- 1
    }
    cache[j]
  }
  next_rn
}
