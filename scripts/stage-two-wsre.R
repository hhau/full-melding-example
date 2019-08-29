library(rstan)
library(futile.logger)

source("scripts/pooled-prior.R")

# set up containers, log files
# keep track of the stage one indices?
log_filename <- paste0("logs/" , Sys.Date(), "-stage-two-wsre-run.log") 
flog.appender(appender.file(log_filename), name = "stage-two-logger")

# read in the stage one samples.
flog.info("Reading ICU model samples")
icu_stage_one_samples <- readRDS("rds/icu-post-samples.rds")
icu_vars <- c("tot.conf[1]", "tot.conf[2]")
icu_phi_samples <- icu_stage_one_samples[[1]][, icu_vars]

n_stage_two_mcmc <- 50

flog.info("Allocating containers")

phi_samples <- array(
  NA,
  dim = c(n_stage_two_mcmc + 1, 1, 2),
  dimnames = list(
    rep("iteration", n_stage_two_mcmc + 1),
    rep("Chain", 1),
    c("phi[1]", "phi[2]")
  )
)

psi_samples <- array(
  NA,
  dim = c(n_stage_two_mcmc + 1, 1, 3),
  dimnames = list(
    rep("iteration", n_stage_two_mcmc + 1),
    rep("Chain", 1),
    c("pi_det", "chi_grp[1]", "chi_grp[2]")
  )
)
accepted_stage_one_indices <- array(
  NA, 
  dim = c(n_stage_two_mcmc + 1, 1, 1),
  dimnames = list(
    rep("iteration", n_stage_two_mcmc + 1),
    rep("Chain", 1),
    c("Stage one index")
  )
)

# initialize
accepted_stage_one_indices[1, 1, 1] <- sample(seq_len(nrow(icu_phi_samples)), size = 1)
                                              
phi_samples[1, 1, ] <- icu_phi_samples[
  accepted_stage_one_indices[1, 1, 1],
]

psi_samples[1, 1, ] <- c(
  rbeta(n = 1, shape1 = 6, shape2 = 4),
  phi_samples[1, 1, 1] + rlnorm(n = 1, meanlog = 4.93 / 1.1, sdlog = 0.17),
  phi_samples[1, 1, 1] + rlnorm(n = 1, meanlog = 7.71 / 1.1, sdlog = 0.23)
)

flog.info("Compiling Stan objects")
# compile various Stan objects
phi_lp_stanfit <- stan(
  "scripts/stan-files/sev-prior-stage-two-phi-lp.stan",
  chains = 1,
  iter = 1
)
psi_step_stanprefit <- stan_model("scripts/stan-files/sev-prior-psi-step.stan")

flog.info("Starting MCMC loop")

# write the loop
for (ii in 2 : (n_stage_two_mcmc + 1)) {
  # sample a stage one index
  proposed_index <- sample(seq_len(nrow(icu_phi_samples)), size = 1)
  current_index <- accepted_stage_one_indices[ii - 1, 1, 1]
  
  # and corresponding stage one phi
  proposed_phi <- icu_phi_samples[proposed_index, ]
  current_phi <- phi_samples[ii - 1, 1, ]

  # see if we accept that phi
  # this will involve the pooled priors, and __both__ prior marginals.
  log_pooled_prior_term <- log_log_pooled_prior(proposed_phi, current_phi)
  
  # this one is upside down because its a marginalised out
  log_icu_prior_term <- log(icu_prior_marginal(current_phi[1], current_phi[2])) - 
    log(icu_prior_marginal(proposed_phi[1], proposed_phi[2]))

  log_sev_prior_term <- log(
    evaluate(sev_prior_wsre, x_nu = current_phi, proposed_phi)
  )
  
  log_sev_prob <- log_prob(
    object = phi_lp_stanfit,
    upars = unconstrain_pars(
      phi_lp_stanfit,
      pars = list(
        x = as.array(proposed_phi),
        pi_det = psi_samples[ii - 1, 1, 1],
        chi_grp = psi_samples[ii - 1, 1, 2:3]
      )
    )
  ) - log_prob(
    object = phi_lp_stanfit,
    upars = unconstrain_pars(
      phi_lp_stanfit,
      pars = list(
        x = as.array(current_phi),
        pi_det = psi_samples[ii - 1, 1, 1],
        chi_grp = psi_samples[ii - 1, 1, 2:3]
      )
    )
  )
  
  log_alpha <- log_pooled_prior_term + 
    log_icu_prior_term + 
    log_sev_prior_term +
    log_sev_prob
  
  if (runif(1) < exp(log_alpha)) {
    phi_samples[ii, 1, ] <- proposed_phi
    accepted_stage_one_indices[ii, 1, 1] <- proposed_index
  } else {
    phi_samples[ii, 1, ] <- current_phi
    accepted_stage_one_indices[ii, 1, 1] <- current_index
  }
  
  # given that phi, sample a stage two psi
  psi_step <- sampling(
    object = psi_step_stanprefit,
    data = list(x = as.array(icu_phi_samples[ii, ])),
    init = list(
      list(
        pi_det = psi_samples[ii - 1, 1, 1],
        chi_grp = psi_samples[ii - 1, 1, 2 : 3]
      )
    ),
    iter = 501,
    warmup = 500,
    chain = 1,
    refresh = 0,
    control = list(max_treedepth = 10)
  )
  
  psi_samples[ii, 1, 1] <- as.array(psi_step, pars = "pi_det")
  psi_samples[ii, 1, 2 : 3] <- as.array(psi_step, pars = "chi_grp")
  
  # gibbs step - so should be fine.
  if (ii %% 10  == 0) {
    flog.info(sprintf("Iteration: %d", ii), name = "stage-two-logger")
  }
}

# write out the stage two samples
flog.info("Writing samples to disk")

saveRDS(
  object = accepted_stage_one_indices,
  file = "rds/wsre-stage-two-stage-one-indices.rds"
)

saveRDS(
  object = phi_samples,
  file = "rds/wsre-stage-two-phi-samples.rds"
)

saveRDS(
  object = psi_samples,
  file = "rds/wsre-stage-two-psi-sample.rds"
)
