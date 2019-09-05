data {
  
}

parameters {
  real <lower = 0> x [2];
  real <lower = 0, upper = 1> pi_det;
  real <lower = x[1]> chi_grp_1;
  real <lower = x[2]> chi_grp_2;
}

model {
  target += beta_lpdf(pi_det |6, 4);
  target += lognormal_lpdf(chi_grp_1 | 4.93, 0.17);
  target += lognormal_lpdf(chi_grp_2 | 7.71, 0.23);

  target += 
    lgamma(chi_grp_1 - 1) - 
    (lgamma(x[1] - 1) + lgamma(chi_grp_1 - x[1] - 1)) + 
    lmultiply(x[1], pi_det) + 
    (chi_grp_1 - x[1]) * log1m(pi_det); // ideally combine with lmultiply, but not possible

  target += 
    lgamma(chi_grp_2 - 1) - 
    (lgamma(x[2] - 1) + lgamma(chi_grp_2 - x[2] - 1)) + 
    lmultiply(x[2], pi_det) + 
    (chi_grp_2 - x[2]) * log1m(pi_det); // ideally combine with lmultiply, but not possible
}
