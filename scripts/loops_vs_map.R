x <- list(
  cars = mtcars,
  flowers = iris
)

rows <- c()

for (index in seq_along(x)) {
    rows[index] <- x[[index]] |> nrow()
}

map_dbl(x, nrow)
