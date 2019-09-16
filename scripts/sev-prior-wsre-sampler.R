library(wsre)
library(purrr)
library(rstan)
library(futile.logger)

flog.info("Compiling Stan model for wsre")

sev_prior_wsre <- stan_model("scripts/stan-files/sev-prior-wsre.stan")

target_dimension <- 2

# choose some phi_1 and phi_2 values
phi_1 <- seq(from = 30, to = 275, length.out = 10)
phi_2 <- seq(from = 500, to = 3000, length.out = 10)

wf_sd <- array(c(phi_1_sd = 25, phi_2_sd = 250), dim = target_dimension)

# form a grid list
wf_mean <- lapply(1 : length(phi_1), function(x) {
  lapply(1 : length(phi_2), function(y) {
    array(c(phi_1 = phi_1[x], phi_2 = phi_2[y]), dim = target_dimension)
  })
})

wf_mean <- do.call(c, wf_mean)

flog.info("Beginning wsre estimates, mclapply is going to eat the log messages")

# estimate away?
wsre_obj <- wsre(
  stanmodel = sev_prior_wsre,
  wf_mean = wf_mean,
  wf_pars = list(
    wf_sd = wf_sd, 
    wf_exponent = 1, 
    target_dimension = target_dimension
  ),
  n_mcmc_samples = 1250,
  flog_threshold = futile.logger::TRACE
)

# there are going to be some errors here, usually try-errors from the bw.SJ.
# Filter them out.

flog.info("Filtering estimates.")

new_estimates <- lapply(wsre_obj$estimates, function(an_estimate) {
  if (class(an_estimate) == "try-error") {
    return(NULL)
  } else {
    an_estimate
  }
})

filter_vec <- which(sapply(new_estimates, is.null))

if (length(filter_vec) == 0) {
  flog.info("Zero estimates filtered, nice!")
} else {
  flog.info(sprintf("%d estimates filtered.", length(filter_vec)))
  new_estimates <- new_estimates[-filter_vec]
}

wsre_obj$estimates <- new_estimates

flog.info("Saving wsre estimate to disk")

saveRDS(
  object = wsre_obj,
  file = "rds/sev-prior-wsre-estimate.rds"
)

