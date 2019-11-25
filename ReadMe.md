# HarvardX Final Report

This is the final report of for the HarvardX Data Science Professional Certificate. It is based on the LendingClub dataset.

WARNING: The repo is large. If you are only interested in the current status, clone with `git clone --depth=1 https://github.com/Emmanuel-R8/HarvardX-LendingClub.git` , or laternatively download as a zip file [](https://github.com/Emmanuel-R8/HarvardX-LendingClub/archive/master.zip).

Pratical notes:

- It is formatted as a bookdown document instead of simple R markdown. This was the easiest way found to get cross-references working. Apart from that, syntax is identical.

- Building the documents is a matter of running `bookdown::render_book("00-index.Rmd", output_format = "bookdown::pdf_book")` from the top directory. The valuation of this work does not require this, but it should work. 

- In addition, the bookdown assumes that the original datasets have already been loaded, converted to R tibbles and saved in the documents subdirectory. This original dataset is a couple of GB, it is reduced to ca.270MB after conversion and compression. The datasets are not included in this Github repo. Source code to download and replicate those steps are given in the report's appendix. It is also included in the R file that carries out the modeling. 




