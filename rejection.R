vMsim <- function(n, kappa) {
  y <- numeric(n)
  for(i in 1:n) {
    ratio <- 0 
    u <- 1
    while(u > ratio) {
      y0 <- runif(1, - pi, pi)
      ratio <- exp(kappa * (cos(y0) - 1))
      u <- runif(1)
    }
    y[i] <- y0
  }
  y
}

vMsim <- function(n, kappa) {
  y <- numeric(n)
  m <- n  
  fact <- 1
  y0 <- runif(m, - pi, pi)
  u <- runif(m)
  j <- 1
  for(i in 1:n) {
    repeat {
      accept <- u[j] <= exp(kappa * (cos(y0[j]) - 1))
      z <- y0[j]
      j <- j + 1
      if(j > m) {
        if(fact == 1) fact <- n / i
        m <- floor(fact * (n - i + 1))
        y0 <- runif(m, - pi, pi)
        u <- runif(m)
        j <- 1
      }
      if(accept) break
    }
    y[i] <- z
  }
  y
}

vMsim_adapt <- function(n, x1, x2, kappa, trace = FALSE) {
  lf <- function(x) kappa * cos(x) 
  lf_deriv <- function(x) - kappa * sin(x)
  a1 <- 2 * kappa / pi
  a2 <- lf_deriv(x1)
  a3 <- lf_deriv(x2)
  a4 <- - a1
  
  b1 <- kappa
  b2 <- lf(x1) - a2 * x1
  b3 <- lf(x2) - a3 * x2
  b4 <- kappa
  
  z0 <- -pi
  z1 <- -pi/2
  z2 <- (b3 - b2) / (a2 - a3)
  z3 <- pi/2
  z4 <- pi
  
  Q1 <- exp(b1) * (exp(a1 * z1) - exp(a1 * z0)) / a1 
  Q2 <- Q1 + exp(b2) * (exp(a2 * z2) - exp(a2 * z1)) / a2
  Q3 <- Q2 + exp(b3) * (exp(a3 * z3) - exp(a3 * z2)) / a3
  c = Q3 + exp(b4) * (exp(a4 * z4) - exp(a4 * z3)) / a4
  
  m <- n  
  fact <- 1
  j <- 1
  count <- 0
  y <- numeric(n)
  uy <- runif(m)
  u <- runif(m)
  for(i in 1:n) {
    repeat {
      u0 <- c * uy[j]
      if(u0 < Q1) {
        z <- log(a1 * exp(-b1) * u0 + exp(a1 * z0)) / a1
        ratio <- exp(lf(z) - a1 * z - b1)
      } else if(u0 < Q2) {
        z <- log(a2 * exp(-b2) * (u0 - Q1) + exp(a2 * z1)) / a2
        ratio <- exp(lf(z) - a2 * z - b2)
      } else if(u0 < Q3) {
        z <- log(a3 * exp(-b3) * (u0 - Q2) + exp(a3 * z2)) / a3
        ratio <- exp(lf(z) - a3 * z - b3)
      } else {
        z <- log(a4 * exp(-b4) * (u0 - Q3) + exp(a4 * z3)) / a4
        ratio <- exp(lf(z) - a4 * z - b4)
      }
      accept <- u[j] <= ratio
      j <- j + 1
      count <- count + 1
      if(j > m) {
        if(fact == 1) fact <- n / i
        m <- floor(fact * (n - i + 1))
        uy <- runif(m)
        u <- runif(m)
        j <- 1
      }
      if(accept) break
    }
    y[i] <- z
  }
  if(trace)
    cat("kappa =", kappa, ", x1 =", x1, ", x2 =", x2, ":", (count - n) / count, "\n")  
  y
}