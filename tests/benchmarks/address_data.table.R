
x <- data.table(a=1:5, b=1:5)
initial_address <- address(x$a)

x[b%%2 == 0, `:=`(c("a"), lapply(.SD, function(x) x *
                                                2)), .SDcols = c("a")]

initial_address == address(x$a)

x[, `:=`(c("a"), lapply(.SD, function(x) x *
                                     2)), .SDcols = c("a")]
initial_address == address(x$a)




x <- data.table(a=1:5, b=1:5)
str(x)
initial_column_addresses <- sapply(x, address)
initial_value_addresses <- sapply(x$a, address)

x[b%%2 == 0, a := a^2]
str(x)
initial_column_addresses == sapply(x, address)
initial_value_addresses == sapply(x$a, address)


x[, a := a*a]
str(x)
initial_column_addresses == sapply(x, address)
initial_value_addresses == sapply(x$a, address)


Rcpp::cppFunction(plugins=c("cpp11"), "void squareCpp(IntegerVector &vec) {for (int &val: vec) {
                  if (val > 2) val *= val;}}")
vec <- 1L:5L
tracemem(vec)
initial_value_addresses <- sapply(vec, address)
squareCpp(vec)
initial_value_addresses == sapply(vec, address)
vec
