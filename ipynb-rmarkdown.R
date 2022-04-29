library(rmarkdown)
library(xfun)
library(reticulate)
use_condaenv(condaenv = "ds01")

nbf <- "data_structures.ipynb"
xfun::file_string(nbf)
nb_rmd = rmarkdown:::convert_ipynb(nbf)
nb_rmd
