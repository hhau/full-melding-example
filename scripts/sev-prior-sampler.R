source("scripts/common/plot-settings.R")

library(rstan)
library(futile.logger)
library(bayesplot)

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

init_list <- lapply(1 : 5, function(x) generate_sub_init())

model_fit <- sampling(
  prefit,
  cores = 5,
  chains = 5,
  iter = 4000 + (50000/5 + 2),
  warmup = 4000,
  control = list(adapt_delta = 0.99, max_treedepth = 12),
  init = init_list
)

unperm_sev_prior_samples <- extract(model_fit, pars = "x", permuted = FALSE)
sev_prior_samples <- extract(model_fit, pars = "x", permuted = TRUE)

bayesplot_theme_set(theme_classic())
bayesplot_theme_replace(
  panel.grid.major = element_line(),
  panel.grid.minor = element_line(linetype = "dashed", size = rel(2/3)),
  legend.text = element_text(size = rel(1.1)),
  legend.title = element_text(size = rel(1.1))
)
sev_prior_check <- bayesplot::mcmc_trace(unperm_sev_prior_samples)

ggsave_halfheight(
  filename = "plots/sev-prior-chain-check.pdf",
  sev_prior_check
)

saveRDS(
  object = sev_prior_samples,
  file = "rds/sev-prior-samples.rds"
)
