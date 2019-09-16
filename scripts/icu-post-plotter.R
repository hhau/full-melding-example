source("scripts/common/plot-settings.R")

library(dplyr)
library(tibble)
library(futile.logger)
library(wesanderson)

flog.info("Reading ICU posterior samples")

icu_post_samples <- readRDS("rds/icu-post-samples.rds")
icu_vars <- c("tot.conf[1]", "tot.conf[2]")
phi_post_samples <- icu_post_samples[[1]] [, icu_vars]

plot_samples <- min(50000, dim(icu_post_samples)[1], dim(phi_post_samples)[1])

plot_tbl <- tibble(
  x = phi_post_samples[1 : plot_samples, 1],
  y = phi_post_samples[1 : plot_samples, 2],
  var = "ICU Posterior"
)

flog.info("Plotting and save ICU posterior sample density")

# pal <- wes_palette("Zissou1", 100, type = "continuous")
pal <- RColorBrewer::brewer.pal(9, "Blues")

p1 <- ggplot(plot_tbl, aes(x = x, y = y)) +
  stat_density_2d(
    aes(fill = stat(density)),
    geom = "raster",
    contour = FALSE
  ) +
  scale_fill_gradientn(colours = pal) +
  xlab(expression(phi[1])) +
  ylab(expression(phi[2])) +
  scale_x_continuous(limits = c(0, 300)) +
  scale_y_continuous(limits = c(0, 3000)) +
  theme(
    legend.position = "none"
  ) +
  ggtitle("ICU posterior")
  NULL

ggsave_halfheight(
  filename = "plots/icu-posterior.pdf",
  plot = p1
)
