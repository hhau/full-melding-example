source("scripts/common/plot-settings.R")

library(futile.logger)
library(abind)
library(dplyr)
library(tibble)

# read an arbitrary number of no_wsre stage two sample files
# stick them in a list
no_wsre_phi_files <- Sys.glob(
  "rds/no-wsre-stage-two-phi-samples*.rds"
)
no_wsre_psi_files <- Sys.glob(
  "rds/no-wsre-stage-two-psi-samples*.rds"
)

no_wsre_phi_sample_list <- lapply(no_wsre_phi_files, function(a_file) {
  readRDS(a_file)
})
no_wsre_psi_sample_list <- lapply(no_wsre_psi_files, function(a_file) {
  readRDS(a_file)
})

no_wsre_phi_samples <- abind(no_wsre_phi_sample_list, along = 1)
no_wsre_phi_data <- bayesplot::mcmc_intervals_data(
  x = no_wsre_phi_samples,
  prob = 0.5,
  prob_outer = 0.95
)
no_wsre_phi_data$wsre <- "No"
no_wsre_psi_samples <- abind(no_wsre_psi_sample_list, along = 1)
no_wsre_psi_data <- bayesplot::mcmc_intervals_data(
  x = no_wsre_psi_samples,
  prob = 0.5,
  prob_outer = 0.95
)
no_wsre_psi_data$wsre <- "No"


# read an arbitrary number of wsre stage two sample files
# stick them in a list
wsre_phi_files <- Sys.glob(
  "rds/wsre-stage-two-phi-samples*.rds"
)
wsre_psi_files <- Sys.glob(
  "rds/wsre-stage-two-psi-samples*.rds"
)

wsre_phi_sample_list <- lapply(wsre_phi_files, function(a_file) {
  readRDS(a_file)
})
wsre_psi_sample_list <- lapply(wsre_psi_files, function(a_file) {
  readRDS(a_file)
})

wsre_phi_samples <- abind(wsre_phi_sample_list, along = 1)
wsre_phi_data <- bayesplot::mcmc_intervals_data(
  x = wsre_phi_samples,
  prob = 0.5,
  prob_outer = 0.95
)
wsre_phi_data$wsre <- "Yes"

wsre_psi_samples <- abind(wsre_psi_sample_list, along = 1)
wsre_psi_data <- bayesplot::mcmc_intervals_data(
  x = wsre_psi_samples,
  prob = 0.5,
  prob_outer = 0.95
)
wsre_psi_data$wsre <- "Yes"

# make a neat plot
plot_tbl <- bind_rows(
  no_wsre_phi_data,
  no_wsre_psi_data,
  wsre_phi_data,
  wsre_psi_data
)
recode_vec <- c(
  "phi_1" = "phi[1]",
  "phi_2" = "phi[2]",
  "chi_grp_1" = "chi[1]",
  "chi_grp_2" = "chi[2]",
  "pi_det" = "pi^{scriptstyle(det)}"
)
# still don't quite know why this works - look up the tidy eval?
plot_tbl$parameter <- plot_tbl$parameter %>% 
  recode(!!!recode_vec)

p1 <- ggplot(
    plot_tbl,
    aes(
      x = parameter,
      grp = interaction(parameter, wsre),
      col = wsre
    )
  ) +
  facet_wrap(
    . ~ parameter,
    nrow = 3,
    scales = "free",
    labeller = label_parsed
  ) +
  geom_boxplot(
    aes(
      ymin = ll,
      lower = l,
      middle = m,
      upper = h,
      ymax = hh
    ),
    stat = "identity"
  ) +
  xlab("") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(), 
    strip.text.x = element_text(size = 12)
  ) +
  scale_discrete_manual(
    aesthetics = "col",
    guide = guide_legend(reverse = TRUE),
    values = c(
      "No" = highlight_col,
      "Yes" = blues[2]
    ),
    labels = c(
      "No" = "No",
      "Yes" = "Yes"
    )
  ) +
  coord_flip() +
  NULL

p1
# consider filtering the stage one samples? MAYBE

ggsave_fullpage(
  file = "plots/melded-posterior-compare.pdf",
  plot = p1
)

# qqplots
qqplot_quantiles <- seq(from = 0.005, to = 0.995, by = 0.005)
