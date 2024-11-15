#include <Rcpp.h>
using namespace Rcpp;

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
NumericVector grad_cpp_t(
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
      yhat[i] += X(j, iii[i]) * beta[j];
    }
  }
  for(int i = 0; i < m; ++i) {
    for(int j = 0; j < p; ++j) {
      grad[j] += X(j, iii[i]) * (yhat[i]- y[iii[i]]);
    }
  }
  return grad / m;
}