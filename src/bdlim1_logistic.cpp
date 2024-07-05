#include <cmath>
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include "BayesLogit/polyagamma_wrapper.h"

#define TWO_PI (2.0 * M_PI)
#define LOG_2PI std::log(TWO_PI)
#define EPSILON std::numeric_limits<double>::min()

inline arma::vec log_likelihood(const arma::vec& y, const arma::mat& design, const arma::vec& regcoef, double inv_sigma, double constant) {
  return constant - 0.5 * arma::square((y - design * regcoef) * inv_sigma);
}

// [[Rcpp::export]]
Rcpp::List bdlim1_logistic_cpp(const arma::vec& y,
                               const arma::mat& design_input,
                               const uint32_t nits,
                               bool REmodel,
                               const uint32_t nRE,
                               const Rcpp::List& w_group_ids,
                               const arma::mat& basis,
                               const arma::mat& w_input,
                               const arma::mat& theta_input,
                               const arma::mat& Edesign,
                               const arma::mat& exposure) {

  Rcpp::List result = Rcpp::List::create(0);

  return result;
}
