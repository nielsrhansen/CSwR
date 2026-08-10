benchTab <- function(x, ...) {
  expr <- as.character(x$expr)
  expr <- paste("`", expr, "`", sep = "")
  x$expr <- expr
  knitr::kable(x, ...)
}

# Set default ggplot2 theme
ggplot2::theme_set(ggplot2::theme_bw())

# Knitr engine for pseudocode blocks.
# Body is algorithmicx-flavoured LaTeX (\begin{algorithm} ... \begin{algorithmic} ...).
# HTML output is rendered client-side by pseudocode.js;
# LaTeX output uses the algorithm + algpseudocode packages.
knitr::knit_engines$set(algorithm = function(options) {
  code <- paste(options$code, collapse = "\n")
  if (knitr::is_html_output()) {
    paste0(
      "```{=html}\n<pre class=\"pseudocode\">\n",
      code,
      "\n</pre>\n```"
    )
  } else if (knitr::is_latex_output()) {
    paste0("```{=latex}\n", code, "\n```")
  } else {
    code
  }
})
