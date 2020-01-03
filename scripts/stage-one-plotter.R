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

icu_phi_prior_samples <- do.call(rbind, lapply(icu_prior_samples, function(a_mcmc_list) {
  a_mcmc_list[, icu_vars]
})) 

icu_post_samples <- readRDS(
  file = "rds/icu-post-samples.rds"
)
icu_phi_post_samples <- do.call(rbind, lapply(icu_post_samples, function(a_mcmc_list) {
  a_mcmc_list[, icu_vars]
}))
  
  

sev_prior_samples <- readRDS(
  file = "rds/sev-prior-samples.rds"
)[[1]]

flog.info("Plotting prior samples")

n_plot_samples <- min(
  50000,
  dim(icu_phi_prior_samples)[1],
  dim(sev_prior_samples)[1],
  dim(icu_phi_post_samples)[1]
)

plot_tbl <- tibble(
  x = c(
    sev_prior_samples[, 1],
    icu_phi_prior_samples[, 1],
    icu_phi_post_samples[, 1]
  ),
  y = c(
    sev_prior_samples[, 2],
    icu_phi_prior_samples[, 2],
    icu_phi_post_samples[, 2]
  ),
  var = rep(
    c(
      "Severity prior",
      "ICU prior",
      "ICU posterior"
    ),
    times = c(
      dim(sev_prior_samples)[1],
      dim(icu_phi_prior_samples)[1],
      dim(icu_phi_post_samples)[1]
    )
  ),
  fill = rep(
    c(
      "prior",
      "prior",
      "posterior"
    ),
    times = c(
      dim(sev_prior_samples)[1],
      dim(icu_phi_prior_samples)[1],
      dim(icu_phi_post_samples)[1]
    )
  )
)

plot_factor <- factor(
  plot_tbl$var,
  levels = c("b" = "Severity prior", "a" = "ICU prior", "c" = "ICU posterior"),
  labels = c(
    "'Severity'~'prior'",
    "'ICU'~'prior'",
    "'ICU'~'posterior'"
  )
)

plot_tbl$plot_factor <- plot_factor

# pal <- wes_palette("Zissou1", 100, type = "continuous")
pal <- RColorBrewer::brewer.pal(9, "Blues")

p1 <- ggplot(plot_tbl, aes(x = x, y = y, col = fill)) +
  stat_density_2d(
    aes(fill = stat(density), col = fill),
    geom = "raster",
    contour = FALSE,
    n = 250
  ) +
  scale_fill_gradientn(colours = pal) +
  scale_x_continuous(limits = c(0, 300), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 3000), expand = c(0, 0)) +
  facet_wrap(
    ~plot_factor,
    nrow = 1,
    labeller = label_parsed
  ) +
  xlab(expression(phi[1])) +
  ylab(expression(phi[2])) +
  labs(fill = "Density") +
  theme(
    # strip.text = element_text(size = 8),
    panel.spacing = unit(1, "lines"),
    legend.text = element_text(size = rel(0.75))
  )

p1

ggsave_halfheight(
  filename = "plots/prior-stage-one-comparison.pdf",
  plot = p1
)

