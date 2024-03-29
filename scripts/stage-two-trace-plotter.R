source("scripts/common/plot-settings.R")

library(futile.logger)
library(abind)
library(dplyr)
library(tibble)

no_wsre_phi_file <- Sys.glob(
  "rds/no-wsre-stage-two-phi-samples-100k-samples-prior-estimates.rds"
)
no_wsre_phi_samples <- readRDS(no_wsre_phi_file)

wsre_phi_file <- Sys.glob(
  "rds/wsre-stage-two-phi-samples-100k-samples-prior-estimates.rds"
)
wsre_phi_samples <- readRDS(wsre_phi_file)

thin_vec <- round(
  seq(from = 0, to = dim(wsre_phi_samples)[1], length.out = 50000)
)
no_wsre_trace <- bayesplot::mcmc_trace_data(
  no_wsre_phi_samples[thin_vec, , ]
)

wsre_trace <- bayesplot::mcmc_trace_data(
  wsre_phi_samples[thin_vec, , ]
)

# these need to be parse'able as expressions for labeller = label_parsed
no_wsre_trace$method <- "'Naive'"
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
    strip.text.y.left = element_text(angle = 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + 
  ylab("") +
  xlab("Iteration") +
  NULL 

ggsave(
  filename = "plots/stage-two-traces.png",
  plot = p1,
  width = 16.2,
  height = 8,
  units = 'cm'
)

presentation_tbl_1 <- plot_tbl %>%
  filter(method == "'Naive'", parameter == "phi[1]")

presentation_plot_1 <- ggplot(presentation_tbl_1, aes(x = iteration, y = value, group = chain, col = chain)) +
  geom_line(alpha = 0.8) +
  scale_discrete_manual(
    aesthetics = "col",
    values = bayesplot:::chain_colors(15),
  ) +
  theme(
    legend.position = "none",
    strip.text.y = element_text(angle = 180),
    strip.text = element_blank(),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_blank()
  ) + 
  ylab(expression(phi)) +
  xlab("Iteration") +
  NULL 

ggsave(
  filename = "plots/stage-two-trace-presentation-one.png",
  plot = presentation_plot_1,
  width = 7,
  height = 3
)

presentation_tbl_2 <- plot_tbl %>%
  filter(method == "'WSRE'", parameter == "phi[1]")

presentation_plot_2 <- ggplot(presentation_tbl_2, aes(x = iteration, y = value, group = chain, col = chain)) +
  geom_line(alpha = 0.8) +
  scale_discrete_manual(
    aesthetics = "col",
    values = bayesplot:::chain_colors(15),
  ) +
  theme(
    legend.position = "none",
    strip.text.y = element_text(angle = 180),
    strip.text = element_blank(),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_blank()
  ) + 
  ylab(expression(phi)) +
  xlab("Iteration") +
  NULL 

ggsave(
  filename = "plots/stage-two-trace-presentation-two.png",
  plot = presentation_plot_2,
  width = 7,
  height = 3
)

