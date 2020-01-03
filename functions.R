print.data.frame <- function(x, ...) {
  print(tibble::as_tibble(x), ...)
}
