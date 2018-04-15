add1 = c("N1", "N1", "N1")
coords1 = c(1, 2, 3)
vals1 = c("a", "b", "c")
extra1 = c("x", "y", "x")

add2 = c("N2", "N2", "N2", "N2")
coords2 = c(2, 3, 4, 5)
vals2 = c("b", "c", "d", "e")
extra2 = c("z", "y", "x", "x")

add3 = c("N3", "N3", "N3")
coords3 = c(1, 3, 5)
vals3 = c("a", "c", "e")
extra3 = c("z", "z", "x")

df1 <- data.frame(add1, coords1, vals1, extra1)
df2 <- data.frame(add2, coords2, vals2, extra2)
df3 <- data.frame(add3, coords3, vals3, extra3)
df1
df2
df3

list_all <- list(df1, df2, df3)
list_all
lst1 <- lapply(list_all, function(x) { names(x)[2] <- 'V1'; x[2:3] })
res <- Reduce(function(...) merge(..., by = 'V1', all = TRUE), lst1)
res[-1] <- lapply(res[-1], as.character)
res[is.na(res)] <- ''
res