library(data.table)
library(devtools)
document()

set.seed(2019)
A <- data.table(a = 1:1e8, b = sample(12:19, 1e7, TRUE), c = sample(12:19, 1e7, TRUE))
B <- data.table(a = sample(A$a, 2e5), d = sample(2:8, 2e5, TRUE))


#bench::mark(A[B, on =.(a)], A[B, on =.(a), .(a,b,c,d)])

rel <- Relationship$new(left = B, right=A)$on(a)
join <- LeftJoin$new(rel)
ujoin <- UpdateJoin$new(rel)

res_left_unkeyed <- bench::mark(dplyr::left_join(B, A, by = c("a")),
                           join$add(b, c),
                           join$add_all(),
                           min_iterations = 10
)

res_left_unkeyed
ggplot2::autoplot(res_left_unkeyed, "violin")
ggsave("tests/benchmark/img/left_unkeyed.png")

res_update_unkeyed <- bench::mark(dplyr::left_join(B, A, by = c("a")),
                                  ujoin$add(b, c),
                                  ujoin$add_all(),
                                  min_iterations = 10
)

res_update_unkeyed
ggplot2::autoplot(res_update_unkeyed, "violin")
ggsave("tests/benchmark/img/update_unkeyed.png")

setkey(x, y)
setkey(y, x)
res_left_keyed <- bench::mark(dplyr::left_join(B, A, by = c("a")),
                         join$add(b, c),
                         join$add_all(),
                         min_iterations = 10
)

res_left_keyed
ggplot2::autoplot(res_left_keyed, "violin")
ggsave("tests/benchmark/img/left_keyed.png")

res_update_keyed <- bench::mark(dplyr::left_join(B, A, by = c("a")),
                                  ujoin$add(b, c),
                                  ujoin$add_all(),
                                  min_iterations = 10
)

res_update_keyed
ggplot2::autoplot(res_update_keyed, "violin")
ggsave("tests/benchmark/img/update_keyed.png")
