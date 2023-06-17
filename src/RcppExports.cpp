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
// int_remove_na
Rcpp::IntegerVector int_remove_na(Rcpp::IntegerVector& x);
RcppExport SEXP _datatools_int_remove_na(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(int_remove_na(x));
    return rcpp_result_gen;
END_RCPP
}
// is_true_or_false
bool is_true_or_false(SEXP& x);
RcppExport SEXP _datatools_is_true_or_false(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(is_true_or_false(x));
    return rcpp_result_gen;
END_RCPP
}
// is_string
bool is_string(SEXP& x);
RcppExport SEXP _datatools_is_string(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP& >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(is_string(x));
    return rcpp_result_gen;
END_RCPP
}
// enquo
SEXP enquo(SEXP expr, SEXP enclos);
RcppExport SEXP _datatools_enquo(SEXP exprSEXP, SEXP enclosSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type expr(exprSEXP);
    Rcpp::traits::input_parameter< SEXP >::type enclos(enclosSEXP);
    rcpp_result_gen = Rcpp::wrap(enquo(expr, enclos));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_datatools_replace_na_false", (DL_FUNC) &_datatools_replace_na_false, 1},
    {"_datatools_int_remove_na", (DL_FUNC) &_datatools_int_remove_na, 1},
    {"_datatools_is_true_or_false", (DL_FUNC) &_datatools_is_true_or_false, 1},
    {"_datatools_is_string", (DL_FUNC) &_datatools_is_string, 1},
    {"_datatools_enquo", (DL_FUNC) &_datatools_enquo, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_datatools(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
