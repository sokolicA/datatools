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
    return (x[0] == 0 || x[0] == 1) && x.size() == 1;
}
