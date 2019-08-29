library(rstan)
library(futile.logger)

flog.info("Compiling basic severity prior Stan model")

prefit <- stan_model("scripts/stan-files/sev-prior.stan")

flog.info("Sampling basic severity prior Stan model")

generate_sub_init <- function() {
  sub_init <- list(
    pi_det = rbeta(n = 1, shape1 = 6, shape2 = 4),
    chi_grp = array(c(
      rlnorm(n = 1, meanlog = 4.93, sdlog = 0.012),
      rlnorm(n = 1, meanlog = 7.71, sdlog = 0.230)
    )),
    x = array(c(
      round(rnorm(n = 1, mean = 75, sd = 10)),
      round(rnorm(n = 1, mean = 1500, sd = 10))
    ))  
  )
}


init_list <- lapply(1 : 6, function(x) generate_sub_init())

model_fit <- sampling(
  prefit,
  cores = 6,
  chains = 6,
  iter = 4000 + (50000/6 + 2),
  warmup = 4000,
  control = list(adapt_delta = 0.99, max_treedepth = 12),
  init = init_list
)

sev_prior_samples <- extract(model_fit, pars = "x")

saveRDS(
  object = sev_prior_samples,
  file = "rds/sev-prior-samples.rds"
)
