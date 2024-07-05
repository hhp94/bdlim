#include <RcppArmadillo.h>
#include <cmath>

// [[Rcpp::depends(RcppArmadillo)]]
#define TWO_PI (2.0 * M_PI)
#define LOG_2PI std::log(TWO_PI)
#define EPSILON std::numeric_limits<double>::min()

inline arma::vec log_likelihood(const arma::vec& y, const arma::mat& design, const arma::vec& regcoef, double inv_sigma, double constant) {
  return constant - 0.5 * arma::square((y - design * regcoef) * inv_sigma);
}

// [[Rcpp::export]]
Rcpp::List bdlim1_gaussian_cpp(const arma::vec& y,
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
  // init design, w, and theta based on input
  arma::mat design = design_input;
  arma::mat w = w_input;
  arma::mat theta = theta_input;

  // init sigma related params
  uint32_t n = y.n_elem;
  double sigma = arma::stddev(y);
  double inv_sigma;
  double sigma_squared = sigma * sigma;
  double constant;
  double shape = 0.5 + n / 2.0;
  double rate;

  // init coef params
  uint32_t n_regcoef = design.n_cols;
  arma::mat V(n_regcoef, n_regcoef);
  arma::vec m(n_regcoef), regcoef(n_regcoef), coef_draw(n_regcoef), diag_update(n_regcoef);

  // init RE params
  double REprec = 0.01;
  double re_shape = 0.5 + nRE / 2.0;
  double re_rate;
  if (REmodel) {
    diag_update.head(nRE).fill(REprec);
    diag_update.tail(n_regcoef - nRE).fill(0.01);
  } else {
    diag_update.fill(0.01);
  }
  arma::uvec RElocation = arma::regspace<arma::uvec>(0, nRE - 1);

  double ll;

  // convert index of group to 0 based and arma::uvec type
  uint16_t n_weight_groups = w_group_ids.size();
  Rcpp::List group_indices_list(n_weight_groups);
  for (uint16_t j = 0; j < n_weight_groups; ++j) {
    group_indices_list[j] = Rcpp::as<arma::uvec>(w_group_ids[j]) - 1;
  }

  arma::uvec Edesign_loc = arma::regspace<arma::uvec>(design.n_cols - n_weight_groups, design.n_cols - 1);

  // init ellipse slice params
  uint32_t df = basis.n_cols;
  uint32_t n_exposures = basis.n_rows;
  arma::rowvec nu(df);
  double threshold, eta, eta_max, eta_min;

  // init w params
  arma::rowvec theta_prop(df);
  arma::vec weighted_exposure(n_exposures);

  // init storage variables
  arma::cube w_keep(n_weight_groups, n_exposures, nits);
  arma::mat regcoef_keep(nits, n_regcoef);
  arma::vec sigma_keep(nits);
  arma::mat ll_all_keep(nits, n);
  arma::vec REprec_keep;
  if (REmodel) {
    REprec_keep.set_size(nits);
  }

  for (uint32_t i = 0; i < nits; ++i) {
    V = arma::symmatu(design.t() * design / sigma_squared);
    V.diag() += diag_update;
    V = arma::inv_sympd(V);

    m = V * (design.t() * y) / sigma_squared;

    coef_draw = Rcpp::rnorm(n_regcoef);
    regcoef = m + arma::chol(V).t() * coef_draw;

    rate = 0.5 + 0.5 * std::pow(arma::norm(y - design * regcoef), 2);
    sigma = 1.0 / std::sqrt(Rcpp::rgamma(1, shape)[0] / rate);

    inv_sigma = 1.0 / sigma;
    sigma_squared = sigma * sigma;

    if (REmodel) {
      re_rate = 0.5 + 0.5 * arma::accu(arma::square(regcoef(RElocation)));
      REprec = Rcpp::rgamma(1, re_shape)[0] / re_rate;
      diag_update.head(nRE).fill(REprec);
    }

    constant = -0.5 * LOG_2PI - std::log(sigma);

    for (uint16_t j = 0; j < n_weight_groups; ++j) {
      const arma::uvec& group_indices = group_indices_list[j];

      ll = arma::sum(log_likelihood(y.elem(group_indices), design.rows(group_indices), regcoef, inv_sigma, constant));
      threshold = ll + std::log(std::max(Rcpp::runif(1, 0.0, 1.0)[0], EPSILON));
      ll = threshold - 1;

      nu = Rcpp::as<arma::rowvec>(Rcpp::rnorm(df));
      eta = Rcpp::runif(1, 0.0, TWO_PI)[0];
      eta_max = eta;
      eta_min = eta_max - TWO_PI;

      while (ll < threshold) {
        theta_prop = theta.row(j) * std::cos(eta) + nu * std::sin(eta);
        w.row(j) = theta_prop * basis.t();
        w.row(j) /= arma::norm(w.row(j));
        w.row(j) *= arma::sign(arma::sum(w.row(j)));

        weighted_exposure = exposure.rows(group_indices) * w.row(j).t();

        design.submat(group_indices, Edesign_loc) = Edesign.rows(group_indices) % arma::repmat(weighted_exposure, 1, Edesign.n_cols);

        ll = arma::sum(log_likelihood(y.elem(group_indices), design.rows(group_indices), regcoef, inv_sigma, constant));

        if (eta < 0) {
          eta_min = eta;
        } else {
          eta_max = eta;
        }
        eta = Rcpp::runif(1, eta_min, eta_max)[0];
      }

      theta.row(j) = theta_prop;
      w_keep.slice(i).row(j) = w.row(j);
    }

    regcoef_keep.row(i) = regcoef.t();
    sigma_keep(i) = sigma;

    if (REmodel) {
      REprec_keep(i) = REprec;
    }

    ll_all_keep.row(i) = log_likelihood(y, design, regcoef, inv_sigma, constant).t();
  }

  Rcpp::List result = Rcpp::List::create(
    // Rcpp::Named("ll") = ll,
    // Rcpp::Named("threshold") = threshold,
    // Rcpp::Named("theta_prop") = theta_prop,
    // Rcpp::Named("w") = w,
    // Rcpp::Named("design") = design,
    // Rcpp::Named("eta") = eta,
    // Rcpp::Named("theta") = theta,
    Rcpp::Named("w_keep") = w_keep,
    Rcpp::Named("regcoef_keep") = regcoef_keep,
    Rcpp::Named("sigma_keep") = sigma_keep,
    Rcpp::Named("ll_all_keep") = ll_all_keep.t()
  );

  if (REmodel) {
    result["REprec_keep"] = REprec_keep;
  }

  return result;
}
