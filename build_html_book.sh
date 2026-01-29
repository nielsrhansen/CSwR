#!/usr/bin/env Rscript

bookdown::render_book(
  output_format = 'bookdown::bs4_book', 
  encoding = 'UTF-8',
  output_dir = "../CSwR_html", 
  clean = TRUE
)