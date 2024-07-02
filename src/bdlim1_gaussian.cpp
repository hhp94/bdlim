#include <RcppArmadillo.h>
#include <cmath>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
Rcpp::List bdlim1_gaussian_cpp(const arma::vec& y, const arma::mat& design, uint32_t nits,
                               bool REmodel, uint32_t nRE,
                               const Rcpp::List& w_group_ids) {
  // Starting values specific for `bdlim1_gaussian`
  uint32_t n = y.n_elem;
  double sigma = arma::stddev(y);
  double sigma_squared = sigma * sigma;

  // Store `n_regcoef`
  uint32_t n_regcoef = design.n_cols;

  // Initialize covariance matrix V
  arma::mat V(n_regcoef, n_regcoef);

  // Initialize REprec
  double REprec = 0.01;
  double re_shape = 0.5 + nRE / 2.0;
  double re_rate;

  // Create the vector for diagonal update
  arma::vec diag_update(n_regcoef);
  if (REmodel) {
    diag_update.head(nRE).fill(REprec);
    diag_update.tail(n_regcoef - nRE).fill(0.01);
  } else {
    diag_update.fill(0.01);
  }

  // Initialize m, regcoef, and coef_draw
  arma::vec m(n_regcoef), regcoef(n_regcoef), coef_draw(n_regcoef);

  // Initialize variables for sigma update
  arma::vec residuals(n);
  double shape = 0.5 + n / 2.0;
  double rate;

  // Create RElocation vector (0 to nRE-1)
  arma::uvec RElocation = arma::regspace<arma::uvec>(0, nRE - 1);

  // Initialize ll
  double ll;

  // Calculate `n_weight_groups`
  uint16_t n_weight_groups = w_group_ids.size();

  // Convert `w_group_ids` to a list of `arma::uvec` to use `arma` methods.
  // decrease indices by 1
  Rcpp::List group_indices_list(n_weight_groups);
  for (uint16_t j = 0; j < n_weight_groups; ++j) {
    group_indices_list[j] = Rcpp::as<arma::uvec>(w_group_ids[j]) - 1;
  }

  // MCMC loop
  for (uint32_t i = 0; i < nits; ++i) {
    // Update `V`
    V = design.t() * design / sigma_squared;
    V.diag() += diag_update;

    // Inverse of symmetric positive definite matrix `V`
    V = arma::inv_sympd(V);

    // Calculate `m`
    m = V * (design.t() * y) / sigma_squared;

    // Update `regcoef`
    coef_draw = Rcpp::rnorm(n_regcoef);
    regcoef = m + arma::chol(V).t() * coef_draw;

    // Update `sigma`
    residuals = y - design * regcoef;
    rate = 0.5 + 0.5 * arma::dot(residuals, residuals);
    sigma = 1.0 / std::sqrt(Rcpp::rgamma(1, shape)[0] / rate);

    // Update `sigma_squared`
    sigma_squared = sigma * sigma;

    // Update random effect variance if a RE model
    if (REmodel) {
      re_rate = 0.5 + 0.5 * arma::accu(arma::square(regcoef(RElocation)));
      REprec = Rcpp::rgamma(1, re_shape)[0] / re_rate;

      // Update the diagonal of `V` corresponding to random effects
      diag_update.head(nRE).fill(REprec);
    }

    // Calculate log likelihood for each weight group
    for (uint16_t j = 0; j < n_weight_groups; ++j) {
      arma::uvec group_indices = group_indices_list[j];
      arma::vec y_group = y.elem(group_indices);
      arma::vec mean_group = design.rows(group_indices) * regcoef;
      arma::vec sigma_vec = arma::vec(y_group.n_elem, arma::fill::value(sigma));
      ll = arma::accu(arma::log_normpdf(y_group, mean_group, sigma_vec));
    }
  }

  return Rcpp::List::create(
    Rcpp::Named("V") = V,
    Rcpp::Named("m") = m,
    Rcpp::Named("regcoef") = regcoef,
    Rcpp::Named("sigma") = sigma,
    Rcpp::Named("REprec") = REprec,
    Rcpp::Named("ll") = ll
  );
}
