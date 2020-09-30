# setwd("/home/ryan/Dropbox/Studies/2020Spring/DiscProj/resources/R_packages")
# getwd()
# create("PageRank")

# Only run this script from the directory, use build.sh
## Load Packages
library(devtools)
library(roxygen2)

## Collect all Package Names
packages <- list.dirs(recursive=FALSE)

## Install Each Package
for (pac in packages) {
  ## Build the Documentation
  setwd(pac)
  document()
  print(paste(pac, "documentation built"))
  setwd("../")
  
  ## Install the package
  install(pac)
  print(paste(pac, "installed"))
}


