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





set.seed(2019)
A <- data.table(a = 1:1e8, b = sample(12:19, 1e7, TRUE), c = sample(12:19, 1e7, TRUE))
dfA <- DF(A)
sfA <- SF(A)

res_filter_before <- bench::mark(
    A[b==15, lapply(.SD, sd)],
    A[b==15][, lapply(.SD, sd)],
    min_iterations = 10
)
ggplot2::autoplot(res_filter_before, "violin")


res_select_before <- bench::mark(
    A[, .SD, .SDcols = c("a", "b")][, lapply(.SD, mean)],
    A[, .(a, b)][, lapply(.SD, mean)],
    A[, c("a", "b")][, lapply(.SD, mean)],
    A[, lapply(.SD, mean), .SDcols = c("a", "b")],
    min_iterations = 10
)
ggplot2::autoplot(res_select_before, "violin")


res_filter_speed <- bench::mark(
    DF(A[5e6:10e7]),
    sfA$filter(5e6:10e7)$data,
    min_iterations = 10
)

res_filter_speed <- bench::mark(
    A[5e6:10e7],
    min_iterations = 10
)
ggplot2::autoplot(res_filter_speed, "violin")


## SF$aggregate vs summarise

set.seed(2019)
A <- data.table(a = 1:1e8, b = sample(12:19, 1e7, TRUE), c = sample(12:19, 1e7, TRUE))
sfA <- SF(A)

res_sfagg_vs_dplyrsummarise <- bench::mark(
    sfA$aggregate(list(mean(x)), columns=is.numeric)$a,
    summarise(A, across(where(is.numeric), mean))$a,
    A[, lapply(.SD, mean), .SDcols=is.numeric]$a,
    min_iterations = 10
)
ggplot2::autoplot(res_sfagg_vs_dplyrsummarise, "violin")

set.seed(2019)
A <- data.table(a = 1:1e8, b = sample(12:19, 1e7, TRUE), c = sample(12:19, 1e7, TRUE))
sfA <- SF(A)
grp_A <- A |> group_by(c)

res_sfagg_vs_dplyrsummarise_grouped <- bench::mark(
    A[, lapply(.SD, mean), .SDcols=is.numeric, keyby = .(c)]$a,
    sfA$aggregate(list(mean(x)), by=.(c))$a,
    summarise(grp_A, across(where(is.numeric), mean))$a,
    summarise(group_by(A, c), across(where(is.numeric), mean))$a,
    min_iterations = 10
)
ggplot2::autoplot(res_sfagg_vs_dplyrsummarise_grouped, "violin")


res_sfagg_vs_dplyrsummarise_filter_grouped <- bench::mark(
    A[c != 15, lapply(.SD, mean), .SDcols=is.numeric, keyby = .(c)]$a,
    sfA$filter(c != 15)$aggregate(list(mean(x)), by=.(c))$a,
    summarise(filter(grp_A, c != 15), across(where(is.numeric), mean))$a,
    summarise(group_by(filter(A, c != 15), c), across(where(is.numeric), mean))$a,
    min_iterations = 10
)
ggplot2::autoplot(res_sfagg_vs_dplyrsummarise_filter_grouped, "violin")


res_max <- bench::mark(
    sfA$max()$b,
    summarise(A, across(where(is.numeric), max))$b,
    min_iterations = 10
)
ggplot2::autoplot(res_max, "violin")

res_sd <- bench::mark(
    summarise(A, across(where(is.numeric), sd))$b,
    sfA$sd()$b,
    min_iterations = 10
)
ggplot2::autoplot(res_sd, "violin")

res_mean <- bench::mark(
    summarise(A, across(where(is.numeric), mean))$b,
    sfA$mean()$b,
    A[, lapply(.SD, mean), .SDcols=is.numeric]$b,
    A[, lapply(.SD, mean, na.rm=T), .SDcols=is.numeric]$b,
    min_iterations = 10
)
ggplot2::autoplot(res_mean, "violin")



## filter by group
res_filt_by_group <- bench::mark(
    A[, .SD[a==max(a)], by=b]$a,
    A[A[, .I[a==max(a)], by=b]$V1]$a,
    A[A[, .I[a==max(a)], by=b][, V1]]$a
)
ggplot2::autoplot(res_filt_by_group, "violin")
