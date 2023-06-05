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
bool is_true_or_false(SEXP& x) {
    return (TYPEOF(x)==LGLSXP && LENGTH(x)==1 && LOGICAL(x)[0]!=NA_LOGICAL);
}
