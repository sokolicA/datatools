test <- Rcpp::cppFunction(
    "
bool compare_chr(CharacterVector& x, CharacterVector& y){
    return x[0] < y[0];
}
    ")



test <- Rcpp::cppFunction(
    "
CharacterVector intersect_chr(CharacterVector& x, CharacterVector& y){
    CharacterVector s;
    CharacterVector l;
    CharacterVector result;

    if(x.size() <= y.size()) {
        s = Rcpp::sort_unique(x);
        l = y
    } else {
        s = Rcpp::sort_unique(y);
        l = x
    }

    for (size_t i {0}; i<length(y); i++) {

    }
}
    ")


test <- Rcpp::cppFunction(
    "
bool test(SEXP expr){
    return TYPEOF(expr) == SYMSXP;
}
    ")


test <- Rcpp::cppFunction(
    "
int test(SEXP& expr){
    return LENGTH(expr);
}
    ")


dataframe <- data.frame(x=1)
test <- Rcpp::cppFunction(
    '
DataFrame makeDT(DataFrame& x){
    StringVector cl {"data.table", "data.frame"};
    x.attr("class") = cl;
    return x;
}
    ')
makeDT(dataframe)
class(makeDT)


test <- Rcpp::cppFunction(
    '
    Rcpp::LogicalVector is_true_or_false(SEXP& x) {
    return TYPEOF(x)==LGLSXP;
}
'
)


test <- Rcpp::cppFunction(
    '
    int sexp_type(SEXP& x) {
    return TYPEOF(x);
}
'
)

test <- Rcpp::cppFunction(
'
SEXP getlang(SEXP& fun, SEXP& args, SEXP& names) {
    SEXP call = PROTECT(Rf_allocVector(LANGSXP, LENGTH(args) + 1));
    SETCAR(call, fun);

    SEXP s = CDR(call);
    SETCAR(s, STRING_ELT(args, 0));

    for (size_t i {1}; i < LENGTH(args); i++) {
        s = CDR(s);
        SETCAR(s, STRING_ELT(args, i));
        SET_TAG(s, Rf_install("arg1"));
    }
    UNPROTECT(1);
    return call;
}
'
)


test <- Rcpp::cppFunction(
    '
SEXP getlang(SEXP& fun, SEXP& args, SEXP& names) {
    SEXP call = PROTECT(Rf_allocVector(LANGSXP, LENGTH(args) + 1));
    SETCAR(call, fun);

    SEXP s = CDR(call);
    SETCAR(s, STRING_ELT(args, 0));

    for (size_t i {1}; i < LENGTH(args); i++) {
        s = CDR(s);
        SETCAR(s, STRING_ELT(args, i));
        SET_TAG(s, Rf_install("arg1"));
    }
    UNPROTECT(1);
    return call;
}
'
)


test <- Rcpp::cppFunction(
    '
    SEXP make_list(SEXP expr, SEXP env) {
        const char *names[] = {"expr", "env", ""};
        SEXP result = PROTECT(Rf_mkNamed(VECSXP, names));  // creates a list of length 2
        SET_VECTOR_ELT(result, 0, expr); // x and y are arbitrary SEXPs
        SET_VECTOR_ELT(result, 1, env);
        UNPROTECT(1);
        return result;
    }
    '
)


Rcpp::cppFunction(
    '
    int parse_i(SEXP expr) {
        return TYPEOF(expr);
    }
    '
)


Rcpp::cppFunction(
    '
    bool parse_i(SEXP expr, SEXP env) {
        bool result;
        try {
            Rf_isFunction(R_eval(expr, env));
        } catch(const std::exception &e) {
            result = false;
        } catch(...) {
            result = false;
        }
        return result;
    }
    '
)


Rcpp::cppFunction(
    '
    bool is_function(SEXP expr, SEXP env) {
        # warning: will be yes even if fun is found in parent env
        int err = 0;
        SEXP check = PROTECT(R_tryEvalSilent(expr, env, &err));
        if (err==1) {
            UNPROTECT(1);
            return false;
        }
        UNPROTECT(1);
        return Rf_isFunction(check);
    }
    '
)


#actually slower
Rcpp::cppFunction(
    '
    int type_of(SEXP expr, SEXP env) {
        # warning: will be yes even if fun is found in parent env
        int err = 0;
        SEXP check = PROTECT(R_tryEvalSilent(expr, env, &err));
        if (err==1) {
            UNPROTECT(1);
            return R_NaInt;
        }
        UNPROTECT(1);
        return TYPEOF(check);
    }
    '
)

r_type_of <- function(expr, env) {
    check <- try(eval(expr, envir = env, enclos=env), silent=TRUE)
    if (inherits(check, "try-error")) return(NA_integer_)
    typeof(check)
}


Rcpp::cppFunction(
    '
    int cpp_lang_length(SEXP s) {
        if (TYPEOF(s) != 6) return R_NaInt;
        int i = 0;
        for(SEXP x = s; x != R_NilValue; x = CDR(x)) {
            i++;
        }
        return i;
    }
    '
)

Rcpp::cppFunction(
    '
    SEXP parse_i(SEXP expr, SEXP names) {
        switch(TYPEOF(expr)) {
            case 0, 10, 13, 14, 16:
                return expr;
                break;
            case 1:
                SEXP check = PROTECT(R_tryEvalSilent(expr))
                break;
        }
        if (TYPEOF(s) == 16) return s;
        if (TYPEOF(s) == 16) return s;
        if (TYPEOF(s) != 6) return R_NaInt;
        int i = 0;
        for(SEXP x = s; x != R_NilValue; x = CDR(x)) {
            i++;
        }
        return i;
    }
    '
)

Rcpp::cppFunction(
    '
    SEXP first_el(SEXP s) {
        return CAR(s);
    }
    '
)

Rcpp::cppFunction(
    '
    SEXP remaining_el(SEXP s) {
        if (TYPEOF(s) != 6) return R_NilValue;
        return CDR(s);
    }
    '
)

Rcpp::cppFunction(
    '
    SEXP remaining_el(SEXP s) {
        if (TYPEOF(s) != 6) return R_NilValue;
        return CDR(s);
    }
    '
)


Rcpp::cppFunction(
    '
SEXP address_of(SEXP x)
{
  // A better way than : http://stackoverflow.com/a/10913296/403310
  char buffer[32];
  snprintf(buffer, 32, "%p", (void *)x);
  return(Rf_mkString(buffer));
}
    '
)

test <- Rcpp::cppFunction(
    '
    SEXP enquo(SEXP expr, SEXP env) {
        const char* names[] = {"expr", "env", ""};
        SEXP result = PROTECT(Rf_mkNamed(VECSXP, names));  // creates a list of length 2
        SET_VECTOR_ELT(result, 0, expr);
        SET_VECTOR_ELT(result, 1, env);
        UNPROTECT(1);
        return result;
    }
    '
)






