// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// replace_na_false
void replace_na_false(Rcpp::LogicalVector& x);
RcppExport SEXP _datatools_replace_na_false(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::LogicalVector& >::type x(xSEXP);
    replace_na_false(x);
    return R_NilValue;
END_RCPP
}
// remove_na_integer
Rcpp::IntegerVector remove_na_integer(Rcpp::IntegerVector& x);
RcppExport SEXP _datatools_remove_na_integer(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(remove_na_integer(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_datatools_replace_na_false", (DL_FUNC) &_datatools_replace_na_false, 1},
    {"_datatools_remove_na_integer", (DL_FUNC) &_datatools_remove_na_integer, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_datatools(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}