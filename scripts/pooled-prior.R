library(dplyr)
library(MASS)
library(spatstat)
library(ks)
library(wsre)
library(futile.logger)

# file needs to be sourced, because I can't figure out how to write
# these file closures to disk at the moment.

flog.info("Loading prior samples, assembling pooled prior.")

model1_im <- readRDS("rds/model1-im.rds")
model1_dprior_im <- readRDS("rds/model1-dprior-im.rds")
model1_kde <- readRDS("rds/model1-kde.rds")

icu_prior_marginal <- function(phi_1, phi_2) {
  model1_dprior_im(phi_1, phi_2)
}

sev_prior_samples <- readRDS("rds/sev-prior-samples.rds")[[1]]
sev_prior_bw <- apply(sev_prior_samples, 2, bw.SJ)

sev_prior_marginal <- function(phi_1, phi_2) {
  wsre:::kde_func_nd(
    x_val = c(phi_1, phi_2), 
    x_sample_mat = sev_prior_samples,
    bw_vec = sev_prior_bw 
  )
}

# for linear pooling, we need to smash together the KDES. 
# I have no interest in anyother linear poolings other than evenly weighted.
log_linear_pooled_prior <- function(phi_nu, phi_de) {
  nu <- log(
    sev_prior_marginal(phi_nu[1], phi_nu[2]) + 
      icu_prior_marginal(phi_nu[1], phi_nu[2]) 
  )
  de <- log(
    sev_prior_marginal(phi_de[1], phi_de[2]) + 
      icu_prior_marginal(phi_de[1], phi_de[2])
  )
  return(nu - de)
}

# for log pooling we should use the wsre for the severity prior.
# not sure it makes much difference for the icu prior.

sev_prior_wsre <- readRDS("rds/sev-prior-wsre-estimate.rds")

log_log_pooled_prior_with_wsre <- function(phi_nu, phi_de) {
  0.5 * (
    log(icu_prior_marginal(phi_nu[1], phi_nu[2])) -
    log(icu_prior_marginal(phi_de[1], phi_de[2]))
  ) +
  0.5 * log(
    evaluate(
      sev_prior_wsre,
      phi_nu,
      phi_de
    )
  )
}

log_log_pooled_prior_no_wsre <- function(phi_nu, phi_de) {
  0.5 * (
    log(icu_prior_marginal(phi_nu[1], phi_nu[2])) -
    log(icu_prior_marginal(phi_de[1], phi_de[2]))
  ) +
  0.5 * (
    log(sev_prior_marginal(phi_nu[1], phi_nu[2])) -
    log(sev_prior_marginal(phi_de[1], phi_de[2]))
  )
}


# log_log_pooled_prior(test_phi_nu, test_phi_de)

flog.info("Pooled priors assembled")
