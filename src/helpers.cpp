#include <Rcpp.h>


// [[Rcpp::export]]
void replace_na_false(Rcpp::LogicalVector& x) {
  for (int i {0}; i < x.size(); i++) {
      if (x[i] == -2147483648) x[i] = 0;
  }
}


// [[Rcpp::export]]
Rcpp::IntegerVector remove_na_integer(Rcpp::IntegerVector& x) {
    return Rcpp::na_omit(x);
}


// [[Rcpp::export]]
Rcpp::LogicalVector is_true_or_false(Rcpp::IntegerVector& x) {
    if (x.size() != 1 || x[0] == -2147483648) return false;
    return (x == 0) | (x == 1);
}
