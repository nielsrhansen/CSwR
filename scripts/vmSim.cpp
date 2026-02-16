#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericVector
von_mises_cpp(int N, double kappa)
{
  NumericVector y(N);
  double y0;
  bool reject;
  for (int i = 0; i < N; ++i) {
    do {
      y0 = R::runif(-M_PI, M_PI);
      reject = R::runif(0, 1) > exp(kappa * (cos(y0) - 1));
    } while (reject);
    y[i] = y0;
  }
  return y;
}
