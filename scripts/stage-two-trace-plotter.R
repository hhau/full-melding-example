source("scripts/common/plot-settings.R")

library(futile.logger)
library(abind)
library(dplyr)
library(tibble)

no_wsre_phi_file <- Sys.glob(
  "rds/no-wsre-stage-two-phi-samples.rds"
)
no_wsre_phi_samples <- readRDS(no_wsre_phi_file)

wsre_phi_file <- Sys.glob(
  "rds/wsre-stage-two-phi-samples.rds"
)
wsre_phi_samples <- readRDS(wsre_phi_file)

no_wsre_trace <- bayesplot::mcmc_trace_data(
  no_wsre_phi_samples
)
wsre_trace <- bayesplot::mcmc_trace_data(
  wsre_phi_samples
)

# these need to be parse'able as expressions for labeller = label_parsed
no_wsre_trace$method <- "'No'~'WSRE'"
wsre_trace$method <- "'WSRE'"

plot_tbl <- bind_rows(
  no_wsre_trace,
  wsre_trace
)

recode_vec <- c(
  "phi_1" = "phi[1]",
  "phi_2" = "phi[2]"
)

plot_tbl$parameter <- plot_tbl$parameter %>% 
  recode(!!!recode_vec)

p1 <- ggplot(plot_tbl, aes(x = iteration, y = value, group = chain, col = chain)) +
  geom_line(alpha = 0.5) +
  facet_grid(
    rows = vars(parameter),
    cols = vars(method),
    scales = "free",
    labeller = label_parsed,
    switch = "y"
  ) +
  scale_discrete_manual(
    aesthetics = "col",
    values = bayesplot:::chain_colors(15),
  ) +
  theme(
    legend.position = "none",
    strip.text.y = element_text(angle = 180)
  ) + 
  ylab("") +
  xlab("Iteration") +
  NULL 

p1

ggsave_halfheight(
  filename = "plots/stage-two-traces.pdf",
  plot = p1
)
