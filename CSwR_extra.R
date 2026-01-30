benchTab <- function(x, ...) {
  expr <- as.character(x$expr)
  expr <- paste("`", expr, "`", sep = "")
  x$expr <- expr
  knitr::kable(x, ...)
}

# Set default ggplot2 theme
ggplot2::theme_set(ggplot2::theme_bw())
