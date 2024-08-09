knots <- c(0, 0, 0, seq(0, 1, 0.2), 1, 1, 1)
xx <- seq(0, 1, 0.005)
B_splines <- splineDesign(knots, xx)[, -c(1, 8)]
matplot(xx, B_splines, type = "l", lty = 1)




fourier <- function(x, p) {
  pp <- 1:p
  x_p <- outer(x, pp)
  cbind(
    cos(x_p),
    sin(x_p)
  )
}

matplot(xx0, fourier(xx0, 10), type = "l", lty = 1)


fourier_d <- function(x, p) {
  pp <- 1:p
  x_p <- outer(x, pp)
  cbind(
    -sin(x_p),
    cos(x_p)
  )
}

fourier_d2 <- function(x, p) {
  pp <- 1:p
  x_p <- outer(x, pp)
  cbind(
    -cos(x_p),
    -sin(x_p)
  )
}



# phipsi score matching with splines 

p <- 25
delta <- 0.001
knots <- c(rep(-pi, 3), seq(-pi, pi, length.out = p), rep(pi, 3))
xx0 <- seq(-pi, pi, delta)

B_splines <- splineDesign(knots, xx0)[, - c(1, p + 2)]
matplot(xx0, B_splines, type = "l", lty = 1)
B_splines_d <- splineDesign(knots, xx0, derivs = 1)[, -c(1, p + 2)]
matplot(xx0, B_splines_d, type = "l", lty = 1)
B_splines_d2 <- splineDesign(knots, xx0, derivs = 2)[, -c(1, p + 2)]
matplot(xx0, B_splines_d2, type = "l", lty = 1)


xx <- phipsi$psi
B_splines_d <- splineDesign(knots, xx, derivs = 1)[, -c(1, p + 2)]
B_splines_d2 <- splineDesign(knots, xx, derivs = 2)[, -c(1, p + 2)]

W <- crossprod(B_splines_d)
d <- - colSums(B_splines_d2)

theta_hat <- solve(W, d)

f_hat <- exp(B_splines %*% theta_hat)
f_hat <- f_hat / sum(f_hat * delta)

hist(xx, prob = TRUE, border = NA)
rug(xx)
lines(xx0, f_hat, type = "l", lwd = 2)


p <- 4
xx <- sort(phipsi$psi)
four_d <- fourier_d(xx, p)
four_d2 <- fourier_d2(xx, p)

W <- crossprod(four_d)
d <- - colSums(four_d2)

theta_hat <- solve(W, d)

f_hat <- exp(fourier(xx0, p) %*% theta_hat)
f_hat <- f_hat / sum(f_hat * delta)

hist(xx, prob = TRUE, border = NA)
rug(xx)
lines(xx0, f_hat, type = "l", lwd = 2)


