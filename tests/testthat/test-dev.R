# > df$select(is.numeric)$select("a")$update(b = 1)
# Warning message:
#     In `[.data.table`(x = private$tbl, j = `:=`(b = 1), .SDcols = "a") :
#     This j doesn't use .SD but .SDcols has been supplied. Ignoring .SDcols. See ?data.table.


