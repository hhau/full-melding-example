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
  n_mcmc_samples = 1e4,
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

# test_phi_nu <- c(phi_1 = 80, phi_2 = 1000)
# test_phi_de <- c(phi_1 = 90, phi_2 = 1500)
# 
# # the 1, 2 things are dodge here
# new_wsre_obj <- wsre_obj
# new_wsre_obj$estimates[1:2] <- NULL
# 
# evaluate(
#   wsre_obj, 
#   test_phi_nu, 
#   test_phi_de
# )
# 
# 
# new_estimate <- new_estimate[-which(sapply(new_estimate, is.null))]
# new_wsre_obj$estimates <- new_estimate
# 
# wsre_obj <- new_wsre_obj
# 
# eval_new <- function (wsre_obj, x_nu, x_de) 
# {
#   stopifnot(length(wsre_obj$estimates[[1]]$wf_pars$wf_mean) == 
#               length(x_nu), length(x_nu) == length(x_de))
#   n_estimates <- length(wsre_obj$estimates)
#   result <- c(NaN)
#   res <- parallel::mclapply(1:n_estimates, mc.cores = 6, function(estimate_index) {
#     with(wsre_obj$estimates[[estimate_index]], {
#       ratio_value <- c(NA)
#       weighting_value <- c(NA)
#       ratio_value <- ratio(x_nu = x_nu, x_de = x_de)
#       if (!wsre:::.is_numerically_okay(ratio_value)) {
#         return(list(weights = NA, w_ratios = NA))
#       }
#       weighting_value <- weighting(x_nu = x_nu, x_de = x_de)
#       weighted_ratio_value <- exp(log(weighting_value) + 
#                                     log(ratio_value))
#       return(list(weight = weighting_value, w_ratio = weighted_ratio_value))
#     })
#   })
#   weights_matrix <- do.call(rbind, lapply(res, function(a_list) {
#     a_list$weight
#   }))
#   ratios_matrix <- do.call(rbind, lapply(res, function(a_list) {
#     a_list$w_ratio
#   }))
#   norm_const <- colSums(weights_matrix, na.rm = TRUE)
#   ratio_value <- colSums(ratios_matrix, na.rm = TRUE)
#   return((1/norm_const) * ratio_value)
# }
# 
# eval_new(new_wsre_obj, test_phi_nu, test_phi_de)
