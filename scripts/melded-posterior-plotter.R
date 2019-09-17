source("scripts/common/plot-settings.R")

library(futile.logger)
library(abind)
library(dplyr)
library(tibble)

# read an arbitrary number of no_wsre stage two sample files
# stick them in a list
no_wsre_files <- Sys.glob(
  "rds/no-wsre-stage-two-phi-samples*.rds"
)

no_wsre_sample_list <- lapply(no_wsre_files, function(a_file) {
  readRDS(a_file)
})

no_wsre_samples <- abind(no_wsre_sample_list, along = 1)
no_wsre_data <- bayesplot::mcmc_intervals_data(
  x = no_wsre_samples,
  prob = 0.5,
  prob_outer = 0.95
)
no_wsre_data$wsre <- "No"

# read an arbitrary number of wsre stage two sample files
# stick them in a list
wsre_files <- Sys.glob(
  "rds/wsre-stage-two-phi-samples*.rds"
)

wsre_sample_list <- lapply(wsre_files, function(a_file) {
  readRDS(a_file)
})

wsre_samples <- abind(wsre_sample_list, along = 1)
wsre_data <- bayesplot::mcmc_intervals_data(
  x = wsre_samples,
  prob = 0.5,
  prob_outer = 0.95
)
wsre_data$wsre <- "Yes"

# make a neat plot
plot_tbl <- bind_rows(no_wsre_data, wsre_data)
recode_vec <- c(
  "phi_1" = "phi[1]",
  "phi_2" = "phi[2]"
)
# still don't quite know why this works - look up the tidy eval?
plot_tbl$parameter <- plot_tbl$parameter %>% 
  recode(!!!recode_vec)

ggplot(plot_tbl, aes(x = parameter, grp = interaction(parameter, wsre), col = wsre)) +
  facet_wrap(
    . ~ parameter,
    nrow = 1,
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
    axis.ticks.y = element_blank()
  ) +
  coord_flip() +
  NULL

# consider filtering the stage one samples? MAYBE

# qqplots
qqplot_quantiles <- seq(from = 0.005, to = 0.995, by = 0.005)
