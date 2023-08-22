# Sort char vector in place.
sort_chr_ <- Rcpp::cppFunction(
    "
    void sort_chr(StringVector& x, bool desc=false){
        x.sort(desc);
    }
    "
    )


intersect_chr <- Rcpp::cppFunction(
    "
CharacterVector intersect_chr(CharacterVector& x, CharacterVector& y){
    return Rcpp::intersect(x, y);
}
    ")


sexp_type <- Rcpp::cppFunction(
# see https://cran.r-project.org/doc/manuals/r-devel/R-ints.html#SEXPTYPEs
'
int sexp_type(SEXP& x) {
    return TYPEOF(x);
}
'
)


