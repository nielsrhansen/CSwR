SG <- function(
  par,
  grad, # Function of parameter and observation index
  N, # Sample size
  gamma, # Decay schedule or a fixed learning rate
  maxit = 100, # Max epoch iterations
  sampler = sample, # How data is resampled. Default is a random permutation
  cb = NULL,
  ...
) {
  gamma <- if (is.function(gamma)) gamma(1:maxit) else rep(gamma, maxit)
  for (k in 1:maxit) {
    if (!is.null(cb) && isTRUE(cb())) {
      break
    }
    samp <- sampler(N)
    for (j in 1:N) {
      i <- samp[j]
      par <- par - gamma[k] * grad(par, i, ...)
    }
  }
  par
}


pois_SG_tracer <- tracer("par", Delta = 10)

SG(
  c(0, 0),
  grad_pois,
  N = N,
  gamma = decay_scheduler(gamma0 = 0.02, gamma1 = 0.001, n1 = 1000),
  maxit = 1000,
  cb = pois_SG_tracer$tracer
)

pois_SG_terminator <- terminator(
  quote(
    {
      gr <- sum(sum(sapply(1:N, \(i) grad(par, i)))^2)
      gr < 0.1
    }
  )
)

pois_SG_terminator$clear()
SG(
  c(0, 0),
  grad_pois,
  N = N,
  gamma = decay_scheduler(gamma0 = 0.02, gamma1 = 0.0001, n1 = 10000),
  maxit = 100000,
  cb = pois_SG_terminator$terminator
)


pois_SG_terminator <- terminator(
  quote({
    n
    gr < 0.1
  }),
  expr = quote({
    if (exists("gr")) {
      gr_old <- gr
    } else {
      gr_old <- NA
    }
    if (!exists("n_old")) {
      n_old <- NA
    }
    gr <- sum(sum(sapply(1:N, \(i) grad(par, i)))^2)
    points(n, log(gr))
    lines(c(n_old, n), log(c(gr_old, gr)))
    n_old <- n
  }),
  N = 10
)


pois_SG_terminator$clear()
SG(
  c(0, 0),
  grad_pois,
  N = N,
  gamma = decay_scheduler(gamma0 = 0.001, gamma1 = 0.00001, n1 = 1000),
  maxit = 100000,
  cb = pois_SG_terminator$terminator
)


## News

ls_model <- function(X, y) {
  N <- length(y)
  X <- unname(X) # Strips X of names
  list(
    # Initial parameter value
    par0 = rep(0, ncol(X)),
    # Objective function
    H = function(beta) {
      drop(crossprod(y - X %*% beta)) / (2 * N)
    },
    # Gradient in a single observation
    grad = function(beta, i) {
      xi <- X[i, ]
      xi * drop(xi %*% beta - y[i])
    }
  )
}

News <- readr::read_csv("data/OnlineNewsPopularity.csv")

News <- dplyr::select(
  News,
  -url,
  -timedelta,
  -is_weekend,
  -n_non_stop_words,
  -n_non_stop_unique_tokens,
  -self_reference_max_shares,
  -kw_min_max
)
# The model matrix without an explicit intercept is constructed from all
# variables remaining in the data set but the target variable 'shares'
X <- model.matrix(shares ~ . - 1, data = News)

lm_News <- lm.fit(X, log(News$shares))
X_raw <- X
# Standardization and log-transforming the target variable
X <- scale(X, center = FALSE)
y <- log(News$shares)
# The '%<-%' destructure assignment operator is from the zeallot package
# c(par0, H, ls_grad) %<-% ls_model(X, y)
tmp <- ls_model(X, y)
par0 <- tmp$par0
H <- tmp$H
ls_grad <- tmp$grad
# Fitting the model using standard linear model computations
lm_News <- lm.fit(X, y)
par_hat <- lm_News$coefficients #


term_cond <- quote(
  {
    h <- H(par) - H(par_hat)
    h < 1e-2
  }
)

news_SG_terminator <- terminator(
  cond = term_cond,
  Delta = 5,
  print = TRUE,
  plotter = plotter("h"),
  xlim = c(0, 200),
  ylim = c(1e-3, 1),
  log = "y"
)

#news_SG_terminator <- terminator(quote({n; h < 0.1}),
#  expr = quote(h <- H(par)), N = 1)

SG_tracer <- tracer(
  "value",
  Delta = 4,
  expr = quote(value <- H(par) - H(par_hat)),
  plotter = plotter("value"),
  xlim = c(0, 200),
  ylim = c(1e-3, 1),
  log = "y"
)

news_SG_terminator$clear(FALSE)
SG_tracer$clear(plotter = plotter("value", col = "red"))
SG(
  par = par0,
  grad = ls_grad,
  N = nrow(X),
  gamma = 3 * 1e-5,
  maxit = 200,
  #cb = SG_tracer$tracer
  cb = news_SG_terminator$terminator
)

# Vector subsetting within function

subset_vec <- function(x, i) {
  xx <- x[i]
  xx / length(xx)
}
subset_vec(1:10)
