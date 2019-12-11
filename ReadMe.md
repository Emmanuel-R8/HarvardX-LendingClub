# HarvardX Final Report

This is the final report of for the HarvardX Data Science Professional Certificate. It is based on the LendingClub dataset.

### WARNING: 

The repo is large. If you are only interested in the current status, clone with `git clone --depth=1 https://github.com/Emmanuel-R8/HarvardX-LendingClub.git` , or aternatively download as a zip file [](https://github.com/Emmanuel-R8/HarvardX-LendingClub/archive/master.zip).


### Pratical notes:

- It is formatted as a bookdown document instead of simple R markdown. This was the easiest way found to get cross-references working. Apart from that, syntax is identical.


- Building the documents is easiest done from the command line running the scripts `rebuild-pdf-book.sh`, or better `nice -n 19 rebuild-pdf-book.sh`. Building from scratch takes a long while. Build cache is of the order of 4GB. Use `nice -n 19 rebuild-git-book.sh` if interested in the gitbook version.


- In addition, the bookdown assumes that the original datasets have already been loaded, converted to R tibbles and saved in the `datasets` subdirectory. This original dataset is a couple of GB and is not included in this Github repo. It is reduced to ca. 270MB after conversion and compression. Source code to download and replicate those steps is in the `Scripts` directory. 

- There may be limits to the amount of data that can be downloaded from GitHub. In case this happens, PM me or lodge an issue. 

- The modeling file (required to be submitted) is named `Model.R`. It is a concatenation of the `05-*-files.Rmd` converted to `.R`.



### Required packages

List of required packages: digest, speedglm, tidyverse, lubridate, gridExtra, kableExtra, lubridate, dslabs, doMC, ROCR.



