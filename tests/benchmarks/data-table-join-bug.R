x <- data.table(x = 1:3, y = LETTERS[1:3], z = LETTERS[9:11], v=1:3)
y <- data.table(x = LETTERS[3:4], y = c(1, 2), z = LETTERS[6:7])
x[, w := y[.SD, on = .(y=x), x.z]]


A <- x
B <- y
A[, w := B[.SD, on = .(y=x), x.z]]

