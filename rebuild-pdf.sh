#!/bin/bash

nice -n 19 R --no-restore --no-save -e 'rm(list = ls(all.names = TRUE)); bookdown::render_book("00-index.Rmd", output_format = "bookdown::pdf_book")'



