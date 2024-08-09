#!/usr/bin/env Rscript

bookdown::render_book(
  output_format = 'bookdown::pdf_book', 
  encoding = 'UTF-8',
  output_dir = "../CSwR_pdf", 
#  clean = TRUE
)