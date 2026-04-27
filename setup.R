# Setup script for R project
# Install required packages

packages <- c("AER", "lmtest", "sandwich", "foreign", "MASS", "ChainLadder", "survival", "mhurdle")

install.packages(packages, repos = "https://cran.rstudio.com/")

# Load packages to check
lapply(packages, library, character.only = TRUE)