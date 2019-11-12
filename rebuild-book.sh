#!/usr/bin/env Rscript

rm(list = ls(all.names = TRUE))
bookdown::render_book("00-index.Rmd", output_format = "bookdown::pdf_book", output_dir = "documents/pdf")
bookdown::render_book("00-index.Rmd", output_format = "bookdown::gitbook", output_dir = "documents/gitbook")



