library(bayesplot)

source("scripts/common/plot-settings.R")

phi_samples <- readRDS("rds/wsre-stage-two-phi-samples.rds")
psi_samples <- readRDS("rds/wsre-stage-two-psi-samples.rds")

phi_trace <- bayesplot::mcmc_trace(phi_samples)
psi_trace <- bayesplot::mcmc_trace(psi_samples)

ggsave_halfheight(
  plot = phi_trace,
  file = "plots/stage-two-wsre-phi-trace.pdf"
)

ggsave_halfheight(
  plot = psi_trace,
  file = "plots/stage-two-wsre-theta-trace.pdf"
)
