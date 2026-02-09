library(profvis)
library(bench)
library(tidyverse)
library(zeallot)

### Data -----

News <- readr::read_csv("data/OnlineNewsPopularity.csv")

News <- dplyr::select(
  News,
  -url,
  -timedelta,
  -weekday_is_sunday,
  -is_weekend
)

X <- model.matrix(shares ~ ., data = News)

sign_sqrt <- function(x) sign(x) * sqrt(abs(x))
X[, -1] <- sign_sqrt(X[, -1])
X[, -1] <- scale(X[, -1])
y <- log(News$shares)

beta <- rep(1, ncol(X))
X <- unname(X)

### ----

source("benchmark_matrix_functions.R")

c(grad1, grad1t, grad, gradt) %<-% grad_models(X, y)

samp <- sample(nrow(X))
i <- 1

mark(
  grad1(beta, samp[i]),
  grad1t(beta, samp[i]),
  grad(beta, samp[i]),
  gradt(beta, samp[i]),
  # i <- i + 1,
  check = TRUE
) %>%
  autoplot()

i <- 1
profvis(
  replicate(
    1e5,
    {
      i <- i + 1
      grad1(beta, samp[i])
      grad1t(beta, samp[i])
      grad(beta, samp[i])
      gradt(beta, samp[i])
      NULL
    }
  ),
  interval = 0.005
)

#### Some benchmarking -----

tbeta <- t(beta)
tX <- t(X)

mark(
  X %*% beta,
  tbeta %*% tX,
  check = FALSE
) %>%
  autoplot()

samp <- sample(nrow(X))
i <- 1:4

mark(
  {
    xi <- X[samp[i], ]
    xi * as.vector(xi %*% beta - y[i])
  },
  {
    xi <- X[samp[i], , drop = FALSE]
    as.vector(crossprod(xi, xi %*% beta - y[i]))
  },
  {
    xi <- X[samp[i], , drop = FALSE]
    drop(crossprod(xi, xi %*% beta - y[i]))
  },
  i <- i + 1,
  check = FALSE
) %>%
  autoplot()
