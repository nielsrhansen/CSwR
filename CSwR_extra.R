benchTab <- function(x, ...) {
  expr <- as.character(x$expr)
  expr <- paste("`", expr, "`", sep = "")
  x$expr <- expr
  knitr::kable(x, ...)
}

# Set default ggplot2 theme
ggplot2::theme_set(ggplot2::theme_bw())

# Minimal theme for schematic/pedagogical plots: axis titles only,
# no grid, no ticks, no panel border. Axis lines on by default.
theme_schematic <- function(
  base_size = 11,
  base_family = "",
  axis_lines = TRUE
) {
  th <- ggplot2::theme_void(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 4)),
      axis.title.y = ggplot2::element_text(
        angle = 90,
        margin = ggplot2::margin(r = 4)
      ),
      plot.background = ggplot2::element_rect(fill = NA, color = NA)
    )
  if (axis_lines) {
    th <- th +
      ggplot2::theme(
        axis.line = ggplot2::element_line(colour = "black", linewidth = 0.3)
      )
  }
  th
}

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
