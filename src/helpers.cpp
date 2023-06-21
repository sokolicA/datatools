#include <Rcpp.h>


// [[Rcpp::export]]
void replace_na_false(Rcpp::LogicalVector& x) {
  for (int i {0}; i < x.size(); i++) {
      if (x[i] == -2147483648) x[i] = 0;
  }
}


// [[Rcpp::export]]
Rcpp::IntegerVector int_remove_na(Rcpp::IntegerVector& x) {
    return Rcpp::na_omit(x);
}

// [[Rcpp::export]]
bool is_true_or_false(SEXP& x) {
    return (TYPEOF(x)==LGLSXP && LENGTH(x)==1 && LOGICAL(x)[0]!=NA_LOGICAL);
}

// [[Rcpp::export]]
bool is_string(SEXP& x) {
    return TYPEOF(x)==STRSXP && LENGTH(x)==1 && STRING_ELT(x, 0)!=NA_STRING;
}

// [[Rcpp::export]]
SEXP enquo(SEXP expr, SEXP env) {
    const char* names[] = {"expr", "env", ""};
    SEXP result = PROTECT(Rf_mkNamed(VECSXP, names));  // creates a list of length 2
    SET_VECTOR_ELT(result, 0, expr);
    SET_VECTOR_ELT(result, 1, env);
    UNPROTECT(1);
    return result;
}
