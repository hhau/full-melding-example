# get devtools first
install.packages("devtools", dependencies = TRUE)
# Mon Sep 16 09:37:29 2019 - installed


# remotes should be pulled as a result of devtools
if (!requireNamespace("remotes")) {
  install.packages("remotes")
}

# then Rcpp - the most recent binary should be fine.
install.packages("Rcpp", dependencies = TRUE)

# then a very specific version of StanHeaders
remotes::install_version(
  "StanHeaders", 
  version = "2.18.1",
  dependencies = TRUE,
  upgrade = "never"
)

# and a specific version of of Stan
remotes::install_version(
  "rstan",
  version = "2.18.1",
  upgrade = "never"
)

# get the packages used in the scripts
# we need a version of jags?? yes, check the modules
install.packages(
  c(
    "dplyr",
    "tibble",
    "futile.logger",
    "wesanderson",
    "coda",
    "rjags",
    "ggplot2",
    "MASS",
    "spatstat",
    "ks",
    "wesanderson",
    "purrr",
    "bayesplot",
    # "parallel",
    "abind"
  )
)

# lastly my stuff
devtools::install_github(
  "hhau/wsre"
)