source("SG_row_column_experiment.R")
Rcpp::sourceCpp("row_column_experiment.cpp")

# News model
X <- model.matrix(shares ~ . - 1, data = CSwR::news)  
X <- X[, -37]
y <- log(CSwR::news$shares)

# Standardization and log-transforming the target variable
X <- scale(X, center = FALSE)
ls_news <- ls_model(X, y)
ls_news_t <- ls_model_transpose(X, y)
lm_news <- lm.fit(X, y)

expr <- quote(value <- ls_news$H(par))
sg_tracer <- CSwR::tracer("value", expr = expr, Delta = 10)
par <- stoch_grad(
  par = ls_news$par0, 
  grad = ls_news$grad, 
  N = ls_news$N, 
  gamma = 1e-5, 
  maxit = 10, 
  m = 500,
  cb = sg_tracer$tracer
)

# gradient-Test

m <- 50
set.seed(10)
grad1 <- ls_news$grad(par, sample(1:ls_news$N, m))
set.seed(10)
grad2 <- ls_news_t$grad(par, sample(1:ls_news$N, m))
set.seed(10)
grad3 <- grad_cpp(par, sample(1:ls_news$N, m), ls_news$X, ls_news$y)
set.seed(10)
grad4 <- grad_cpp_t(par, sample(1:ls_news$N, m), ls_news_t$X, ls_news_t$y)
cbind(grad1, grad2, grad3, grad4)
identical(grad1, grad2)
identical(grad1, grad3)
identical(grad1, grad4)

# gradient-benchmark

bench::mark(
  ord = ls_news$grad(par, sample(1:ls_news$N, m)),
  trans = ls_news_t$grad(par, sample(1:ls_news$N, m)),
  ord_rcpp = grad_cpp(par, sample(1:ls_news$N, m), ls_news$X, ls_news$y),
  trans_rcpp = grad_cpp_t(par, sample(1:ls_news$N, m), ls_news_t$X, ls_news_t$y),
  check = FALSE
)


# SG-Test

set.seed(10)
par1 <- stoch_grad(
  par = ls_news$par0, 
  grad = ls_news$grad, 
  N = ls_news$N, 
  gamma = 1e-5, 
  maxit = 1, 
  m = 500
)
set.seed(10)
par2 <- stoch_grad(
  par = ls_news_t$par0, 
  grad = ls_news_t$grad, 
  N = ls_news_t$N, 
  gamma = 1e-5, 
  maxit = 1, 
  m = 500
)
set.seed(10)
par3 <- stoch_grad(
  par = ls_news$par0, 
  grad = grad_cpp, 
  N = ls_news$N, 
  gamma = 1e-5, 
  maxit = 1, 
  m = 500,
  X = ls_news$X,
  y = ls_news$y
)
set.seed(10)
par4 <- stoch_grad(
  par = ls_news_t$par0, 
  grad = grad_cpp_t,, 
  N = ls_news_t$N, 
  gamma = 1e-5, 
  maxit = 1, 
  m = 500,
  X = ls_news_t$X,
  y = ls_news_t$y
)

cbind(par1, par2, par3, par4)

# SG-Benchmark

bench::mark(
  ord = stoch_grad(
    par = ls_news$par0, 
    grad = ls_news$grad, 
    N = ls_news$N, 
    gamma = 1e-5, 
    maxit = 100, 
    m = 500
  ),
  trans = stoch_grad(
    par = ls_news_t$par0, 
    grad = ls_news_t$grad, 
    N = ls_news_t$N, 
    gamma = 1e-5, 
    maxit = 100, 
    m = 500
  ),
  ord_rcpp = stoch_grad(
    par = ls_news$par0, 
    grad = grad_cpp, 
    N = ls_news$N, 
    gamma = 1e-5, 
    maxit = 100, 
    m = 500,
    X = ls_news$X,
    y = ls_news$y
  ),
  trans_rcpp = stoch_grad(
    par = ls_news_t$par0, 
    grad = grad_cpp_t,, 
    N = ls_news_t$N, 
    gamma = 1e-5, 
    maxit = 100, 
    m = 500,
    X = ls_news_t$X,
    y = ls_news_t$y
  ),
  check = FALSE
)









sg_trace_low <- summary(sg_tracer)
tail(sg_trace_low)
ls_news$H(lm_news$coefficients)
ls_news_t$H(lm_news$coefficients)

sg_tracer <- CSwR::tracer("value", expr = expr)
stoch_grad(
  par = ls_news_t$par0, 
  grad = ls_news_t$grad, 
  N = ls_news_t$N, 
  gamma = 1e-5, 
  maxit = 50, 
  cb = sg_tracer$tracer
)

p <- profvis::profvis(
  stoch_grad(
    par = ls_news$par0, 
    grad = ls_news$grad, 
    N = ls_news$N, 
    gamma = 1e-5, 
    maxit = 50000,
    m = 500
  )
)
p

p <- profvis::profvis(
  stoch_grad(
    par = ls_news_t$par0, 
    grad = ls_news_t$grad, 
    N = ls_news_t$N, 
    gamma = 1e-5, 
    maxit = 50000, 
    m = 500
  )
)
p

<- profvis::profvis(
  stoch_grad(
    par = ls_news_t$par0, 
    grad = ls_news_t$grad, 
    N = ls_news_t$N, 
    gamma = 1e-5, 
    maxit = 50000, 
    m = 500
  )
)
p

i <- sample(1:ls_news$N, 1000)

bench::mark(
  ls_news$X[i, ],
  ls_news_t$X[, i],
  check = FALSE
)
