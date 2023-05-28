# Data Manipulation Tools

## TODO

DataFrame:

- add number of last updated rows after eval .Last.updated ?

- can this be done in Cpp? str2lang(paste0("c(", paste0("'", cols, "'", collapse = ","), ")"))

- sort by group: x <- data.table(c(3,2,1,2,1), c("a", "a", "a", "b", "b")); x[order(-V1), .SD, by = "V2"]

- add concatenate rcpp: concatenate(vec, sep=",", prefix="", suffix=""): concatenate(1:2, ",")-> "1,2"; concatenate(1:2, ",", "'", "'")-> "'1','2'"

StatFrame:

- Add correlations.

- Think about adding `Missing` class

- Think about adding ggplot support.
