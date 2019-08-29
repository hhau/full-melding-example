#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
double kernel_func_nd(
  NumericVector x_val, 
  NumericVector x_sample,
  NumericVector bw_vec
) {
  NumericVector scaled_vec(x_val.size());
  double result(1); 
  scaled_vec = (x_val - x_sample) / bw_vec;
  result = exp(sum(dnorm(scaled_vec, 0.0, 1.0, true)));
  return(result);
}

// [[Rcpp::export]]
double kde_func_nd(
  NumericVector x_val,
  NumericMatrix x_sample_mat,
  NumericVector bw_vec
) {
  int n_dim = bw_vec.size();
  int n_samples = x_sample_mat.nrow();
  
  NumericVector temp_result(x_sample_mat.nrow());
  double result(1);
  
  double bw_prod(1);
  bw_prod = 1.0;

  for (int dim_index = 0; dim_index < n_dim; ++dim_index) {
    bw_prod = bw_vec[dim_index] * bw_prod;
  }

  for (int sample_index = 0; sample_index < n_samples; ++sample_index) {
    NumericVector a_sample = x_sample_mat(sample_index, _);
    temp_result[sample_index] = kernel_func_nd(
      x_val,
      a_sample,
      bw_vec
    );
  }

  result = sum(temp_result) / (n_samples * bw_prod);
  return(result);
}

// [[Rcpp::export]]
double pointwise_weighting_function_nd(
  NumericVector x_sample,
  NumericVector wf_mean,
  NumericVector wf_sd,
  double wf_exponent, // this needs to be one to precisely control the product mean
  bool log_scale
) {
  double result(1);
  NumericVector scaled_vec = (x_sample - wf_mean) / wf_sd;

  if (log_scale) {
    result = sum(wf_exponent * dnorm(scaled_vec, 0.0, 1.0, true));   
  } 

  return(result);
}

// [[Rcpp::export]]
double weight_gauss_kde_jones_nd(
  NumericVector x,
  NumericMatrix weighted_samples,
  NumericVector wf_mean,
  NumericVector wf_sd,
  double wf_exponent,
  NumericVector bandwidth
) {
  int n_dim = bandwidth.size();
  int n_samples = weighted_samples.nrow();
  double result(1);
  double bw_prod = 1.0;
  NumericVector log_wf_inv_vals(n_samples);
  NumericVector log_kernel_vals(n_samples);

  for (int sample_index = 0; sample_index < n_samples; ++sample_index) {
    log_wf_inv_vals[sample_index] = -1.0 * pointwise_weighting_function_nd(
      weighted_samples(sample_index, _),
      wf_mean,
      wf_sd,
      wf_exponent,
      true
    );

    log_kernel_vals[sample_index] = log(kernel_func_nd(
      x,
      weighted_samples(sample_index, _),
      bandwidth
    ));
  }

  for (int dim_index = 0; dim_index < n_dim; ++dim_index) {
    bw_prod = bandwidth[dim_index] * bw_prod;
  }

  result = sum(exp(log_kernel_vals + log_wf_inv_vals));
  return(result / (n_samples * bw_prod));
}
