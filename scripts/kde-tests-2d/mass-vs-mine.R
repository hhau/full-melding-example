library(Rcpp)
library(MASS)
library(mvtnorm)

# test some code vs kde2d
sourceCpp("scripts/kde-tests-2d/kde-new.cpp")

# simulate some 2D cata
x_sample_mat <- rmvnorm(n = 500, mean = c(0, 1), sigma = matrix(c(1, 0.5 * 1 * 2, 0.5 * 1 * 2, 2), nrow = 2))

# fix some bandwidths
h_1 <- bw.SJ(x_sample_mat[, 1])
h_2 <- bw.SJ(x_sample_mat[, 2])

# see what kde2d gives
kde2d_res <- kde2d(
  x = x_sample_mat[, 1],
  y = x_sample_mat[, 2],
  h = 4 * c(h_1, h_2),
  n = 2^5,
  lims = c(-3, 4, -3, 4)
)
# contour(kde2d_res)

# check to see that I get the same thing
x_vals <- seq(from = -3, to = 4, length.out = 2^5)
x_mat <- as.matrix(expand.grid(x = x_vals, y = x_vals)) 
bw_vec <- c(h_1, h_2)

kernel_func <- function(x_val, x_sample, bw_vec) {
  scaled_vec <- (x_val - x_sample) / bw_vec
  res <- exp(sum(dnorm(scaled_vec, log = T)))
  return(res)
}

kde_func <- function(x_val, x_sample_mat, bw_vec) {
  res <- apply(x_sample_mat, 1, function(a_sample) {
    kernel_func(x_val, a_sample, bw_vec)
  })
  f_res <- sum(res)  / (nrow(x_sample_mat) * prod(bw_vec))
  return(f_res)
}

kde_nd_mine <- function(x_mat, x_sample_mat, bw_vec) {
  n_x_vals <- nrow(x_mat)
  n_samples <- nrow(x_sample_mat)
  n_dim <- ncol(x_mat)
  
  stopifnot(exprs = {
    n_dim == length(bw_vec) # have a bandwidth for each dimension
    n_dim == ncol(x_sample_mat) # nothing silly across dimensions
  })
  
  res <- apply(x_mat, 1, function(a_value) {
    kde_func(a_value, x_sample_mat, bw_vec)
  })
  
  return(res)
}

plot_df <- as.data.frame(x_mat)
plot_df$my_kde_res <- apply(X = x_mat, 1, function(x) kde_func_nd(x, x_sample_mat, bw_vec))

withr::with_par(new = list(mfrow = c(2, 1)), {
  contour(kde2d_res, main = "MASS kde2d")
  contour(
    x = x_vals,
    y = x_vals,
    z = matrix(plot_df$my_kde_res, nrow = length(x_vals), byrow = F),
    main = "My KDE2d"
  )
})

# woo! 
all.equal(
  target = kde2d_res$z,
  current = matrix(plot_df$my_kde_res, nrow = length(x_vals), byrow = F)
)

library(bench)
bench_res <- bench::mark(
  apply(x_mat, 1, function(x) kde_func_nd(x, x_sample_mat, bw_vec)),
  kde_nd_mine(x_mat, x_sample_mat, bw_vec),
  iterations = 3
)
plot(bench_res)


# 1d tests
x_samples <- rnorm(n = 100)
h_1d <- bw.SJ(x_samples)

res <- density(
  x_samples,
  bw = "SJ",
  kernel = "gaussian",
  n = 512,
  from = -3, 
  to = 4
)

density_builtin <- res$y
density_mine <- apply(as.matrix(x_vals), 1, function(x) kde_func_nd(x, as.matrix(x_samples), h_1d))
density_orig <- wsre:::gauss_kde(x_vals, x_samples, h_1d)

plot(
  x = res$x,
  y = density_builtin,
  type = "l"
)

lines(
  x = x_vals,
  y = density_mine,
  type = "l",
  col = "red"
)

lines(
  x = x_vals,
  y = density_orig,
  type = "l",
  col = "blue"
)

# test to see if we get back the same thing from previous version of WSRE
# for 1d case
library(wsre)
 
# target is N(0, 1)
test_point <- 0.1
wf_mean <- 2
wf_sd <- 1
wf_exponent <- 1
weighted_samples <- as.matrix(rnorm(n = 500, mean = 1, sd = 0.5))
a_bw <- bw.SJ(weighted_samples)

wsre:::weight_gauss_kde_jones(
  x = test_point,
  weighted_samples = weighted_samples,
  wf_mean = wf_mean,
  wf_sd = wf_sd,
  wf_exponent = wf_exponent,
  bandwidth = a_bw
)

weight_gauss_kde_jones_nd(
  x = test_point,
  weighted_samples = as.matrix(weighted_samples),
  wf_mean = wf_mean,
  wf_sd = wf_sd,
  wf_exponent = wf_exponent,
  bandwidth = a_bw
)

# the above don't agree, why - have now fixed


# check the weighting functions
pointwise_weighting_function_nd(
  x_sample = weighted_samples[1, ],
  wf_mean = wf_mean,
  wf_sd = wf_sd, 
  wf_exponent = wf_exponent, 
  TRUE
)

wsre:::weighting_function(
  weighted_samples[1, ],
  wf_mean,
  wf_sd,
  wf_exponent,
  TRUE
)
# these are fine

# onto the kdes - seem absolutely fine
kde_func_nd(
  test_point,
  weighted_samples,
  bw_vec = a_bw
)

wsre:::gauss_kde(
  test_point, 
  as.vector(weighted_samples),
  a_bw
)

# check we get something sensible for 2D cases
test_point <- c(0.1, 0.25)
weighted_samples <- rmvnorm(n = 500, mean = c(1, 1), sigma = matrix(c(0.5, 0, 0, 0.5), nrow = 2))
wf_mean <- c(2, 2)
wf_sd <- c(1, 1)
wf_exponent <- 1
bw_vec <- apply(weighted_samples, 2, bw.SJ)

weight_gauss_kde_jones_nd(
  x = test_point,
  weighted_samples = as.matrix(weighted_samples),
  wf_mean = wf_mean,
  wf_sd = wf_sd,
  wf_exponent = wf_exponent,
  bandwidth = bw_vec
)

