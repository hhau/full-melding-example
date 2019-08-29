data {

}

parameters {
  real <lower = 0, upper = 1> pi_det;
  real <lower = 0> chi_grp [2];
  real <lower = 0> x [2];
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
}