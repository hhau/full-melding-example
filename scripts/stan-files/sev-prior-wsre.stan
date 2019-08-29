data {
  int <lower = 1> target_dimension; // strictly two in this example.
  real wf_mean [target_dimension];
  real <lower = 0> wf_sd [target_dimension];
  real <lower = 0> wf_exponent;
}

parameters {
  real <lower = 0, upper = 1> pi_det;
  real <lower = 0> chi_grp [target_dimension];
  // x == tot.conf[1:2] in other code.
  real <lower = 0> x [target_dimension];
}

model {
  pi_det ~ beta(6, 4);
  chi_grp[1] ~ lognormal(4.93, 0.17);
  chi_grp[2] ~ lognormal(7.71, 0.23);

  target += 
    lgamma(chi_grp[1] - 1) - 
    (lgamma(x[1] - 1) + lgamma(chi_grp[1] - x[1] - 1)) + 
    lmultiply(x[1], pi_det) + 
    (chi_grp[1] - x[1]) * log1m(pi_det); // ideally combine with lmultiply, but not possible

  target += 
    lgamma(chi_grp[2] - 1) - 
    (lgamma(x[2] - 1) + lgamma(chi_grp[2] - x[2] - 1)) + 
    lmultiply(x[2], pi_det) + 
    (chi_grp[2] - x[2]) * log1m(pi_det); // ideally combine with lmultiply, but not possible

  // wsre bit
  target += wf_exponent * normal_lpdf(x | wf_mean, wf_sd);
}
