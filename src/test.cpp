#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
void replace_na_false(LogicalVector& x) {
  for (int i {0}; i < x.size(); i++) {
      if (x[i] == NA_LOGICAL) x[i] = 0;
  }
}


// [[Rcpp::export]]
IntegerVector remove_na_integer(IntegerVector& x) {
    return na_omit(x);
}

