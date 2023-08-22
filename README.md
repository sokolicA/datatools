# Wrapper around tabular

## Considerations

- By group operations: x <- data.table(c(3,2,1,2,1), c("a", "a", "a", "b", "b")); x[order(-V1), .SD, by = "V2"]

- Validators.

- Stat functions.

- Missing data analysis.

- Ggplot support.
