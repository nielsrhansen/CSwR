benchTab <- function(x, ...) {
  expr <- as.character(x$expr)
  expr <- paste("`", expr, "`", sep = "")
  x$expr <- expr
  knitr::kable(x, ...)
}