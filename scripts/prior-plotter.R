source("scripts/common/plot-settings.R")

library(dplyr)
library(tibble)
library(futile.logger)
library(wesanderson)

flog.info("Reading prior samples")

icu_prior_samples <- readRDS(
  file = "rds/icu-prior-samples.rds"
)

icu_vars <- c("tot.conf[1]", "tot.conf[2]")
icu_phi_samples <- icu_prior_samples[[1]][, icu_vars]

sev_prior_samples <- readRDS(
  file = "rds/sev-prior-samples.rds"
)[[1]]

flog.info("Plotting prior samples")

n_plot_samples <- min(50000, dim(icu_phi_samples)[1], dim(sev_prior_samples)[1])

plot_tbl <- tibble(
  x = c(
    sev_prior_samples[1 : n_plot_samples, 1],
    icu_phi_samples[1 : n_plot_samples, 1]
  ),
  y = c(
    sev_prior_samples[1 : n_plot_samples, 2],
    icu_phi_samples[1 : n_plot_samples, 2]
  ),
  var = rep(
    c(
      "Severity Prior",
      "ICU Prior"
    ),
    each = n_plot_samples 
  )
)

# pal <- wes_palette("Zissou1", 100, type = "continuous")
pal <- RColorBrewer::brewer.pal(9, "Blues")

p1 <- ggplot(plot_tbl, aes(x = x, y = y)) +
  stat_density_2d(
    aes(fill = stat(density)),
    geom = "raster",
    contour = FALSE
  ) +
  scale_fill_gradientn(colours = pal) +
  scale_x_continuous(limits = c(0, 300)) +
  scale_y_continuous(limits = c(0, 3000)) +
  facet_wrap(~ var, nrow = 1) +
  xlab(expression(phi[1])) +
  ylab(expression(phi[2])) +
  labs(fill = "Density") 

ggsave_halfheight(
  filename = "plots/prior-comparison.pdf",
  plot = p1
)
