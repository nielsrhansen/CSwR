#include <RcppArmadillo.h>
#include <dqrng.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
NumericVector grad_cpp(
    NumericVector beta, 
    IntegerVector ii, 
    NumericMatrix X, 
    NumericVector y
) {
  int m = ii.length(), p = beta.length();
  NumericVector grad(p), yhat(m);
  // Shift indices one down due to zero-indexing in C++
  IntegerVector iii = clone(ii) - 1;  
  
  for(int i = 0; i < m; ++i) {
    for(int j = 0; j < p; ++j) {
      yhat[i] += X(iii[i], j) * beta[j];
    }
  }
  for(int i = 0; i < m; ++i) {
    for(int j = 0; j < p; ++j) {
      grad[j] += X(iii[i], j) * (yhat[i]- y[iii[i]]);
    }
  }
  return grad / m;
}


// [[Rcpp::export]]
NumericVector batch_cpp(
    NumericVector par, 
    IntegerVector ii, 
    double gamma, 
    NumericMatrix X, 
    NumericVector y, 
    int m = 50
) {
  int p = par.length(), N = ii.length();
  int M = floor(N / m);
  NumericVector grad(p), yhat(N), beta = clone(par);
  IntegerVector iii = clone(ii) - 1;  
  
  for(int j = 0; j < M; ++j) {
    for(int i = j * m; i < (j + 1) * m; ++i) {
      for(int k = 0; k < p; ++k) {
        yhat[i] += X(iii[i], k) * beta[k];
      }
    }
    for(int k = 0; k < p; ++k) {
      grad[k] = 0;
      for(int i = j * m; i < (j + 1) * m; ++i) {
        grad[k] += X(iii[i], k) * (yhat[i] - y[iii[i]]);
      }
    }
    beta = beta - gamma * (grad / m);
  }
  return beta;
}

// [[Rcpp::depends(dqrng)]]
// [[Rcpp::export]]
NumericVector sg_cpp(
    NumericVector par, 
    int N, 
    NumericVector gamma,
    NumericMatrix X, 
    NumericVector y,
    int m = 50, 
    int maxit = 100
) {
  int p = par.length(), M = floor(N / m);
  NumericVector grad(p), yhat(N), beta = clone(par);
  IntegerVector ii;
  
  for(int l = 0; l < maxit; ++l) {
    // Note that dqsample_int samples from {0, 1, ..., N - 1}
    ii = dqrng::dqsample_int(N, N); 
    for(int j = 0; j < M; ++j) {
      for(int i = j * m; i < (j + 1) * m; ++i) {
        yhat[i] = 0;
        for(int k = 0; k < p; ++k) {
          yhat[i] += X(ii[i], k) * beta[k];
        }
      }
      for(int k = 0; k < p; ++k) {
        grad[k] = 0;
        for(int i = j * m; i < (j + 1) * m; ++i) {
          grad[k] += X(ii[i], k) * (yhat[i] - y[ii[i]]);
        }
      }
      beta = beta - gamma[l] * (grad / m);
    }
  }
  return beta;
}

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::colvec sg_arma(
    NumericVector par, 
    int N, 
    NumericVector gamma,
    const arma::mat& X, 
    const arma::colvec& y, 
    int m = 50, 
    int maxit = 100
) {
  int p = par.length(), M = floor(N / m);
  arma::colvec grad(p), res(N), yhat(N), beta = clone(par);
  uvec ii, iii;
  
  for(int l = 0; l < maxit; ++l) {
    ii = as<arma::uvec>(dqrng::dqsample_int(N, N));
    for(int j = 0; j < M; ++j) {
      iii = ii.subvec(j * m, (j + 1) * m - 1);
      res = X.rows(iii) * beta - y(iii);
      beta = beta - gamma[l] * (X.rows(iii).t() * res / m); 
    }
  }
  return beta;
}