library(MASS)
library(spatstat)
library(ks)
library(futile.logger)

flog.info("Reading samples")

icu_prior_samples <- readRDS(file = "rds/icu-prior-samples.rds")

# this is the one of interest?
icu_prior_variable1 <- paste0("tot.conf[1]")
icu_prior_samples_link1 <- as.numeric(icu_prior_samples[[1]][, icu_prior_variable1])

icu_prior_variable2 <- paste0("tot.conf[2]")
icu_prior_samples_link2 <- as.numeric(icu_prior_samples[[1]][, icu_prior_variable2])

icu_prior_samples_link <- cbind(
  icu_prior_samples_link1,
  icu_prior_samples_link2
)

flog.info("Estimating KDE")

model1_kde <- kde2d(
  x = icu_prior_samples_link[, 1],
  y = icu_prior_samples_link[, 2],
  n = 2 * c(500, 7500),
  lims = c(0, 500, 0, 7500)
)

flog.info("Interpolating KDE")

model1_im <- im(t(model1_kde$z),
                xcol = model1_kde$x,
                yrow = model1_kde$y)

model1_dprior_im <- function(x, y){
  interp.im(model1_im, x, y)
}

flog.info("Writing samples + interpolation + function closure to disk")

saveRDS(
  model1_dprior_im,
  file = "rds/model1-dprior-im.rds"
)
saveRDS(
  model1_kde,
  file = "rds/model1-kde.rds"
)
saveRDS(
  model1_im,
  file = "rds/model1-im.rds"
)