#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include <cmath>

#define TWO_PI (2.0 * M_PI)
#define LOG_2PI std::log(TWO_PI)
#define EPSILON std::numeric_limits<double>::min()

arma::vec dnorm_cpp(const arma::vec& y, const arma::mat& design, const arma::vec& regcoef, double inv_sigma, double constant) {
  return constant - 0.5 * arma::square((y - design * regcoef) * inv_sigma);
}

// [[Rcpp::export]]
Rcpp::List bdlim1_gaussian_cpp(const arma::vec& y,
                               arma::mat& design,
                               const uint32_t nits,
                               bool REmodel,
                               const uint32_t nRE,
                               double REprec,
                               const Rcpp::List& w_group_ids,
                               const arma::mat& Edesign,
                               const arma::mat& basis,
                               arma::mat& w,
                               arma::mat& theta,
                               const arma::mat& exposure)  {
  // init sigma related params
  int n = y.n_elem;
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
  double re_shape = 0.5 + nRE / 2.0;
  double re_rate;
  if (REmodel) {
    diag_update.head(nRE).fill(REprec);
    diag_update.tail(n_regcoef - nRE).fill(0.01);
  } else {
    diag_update.fill(0.01);
  }
  arma::uvec RElocation = arma::regspace<arma::uvec>(0, nRE - 1);

  // init group wise params
  double ll;

  // init pointers to the indices of the groups (which rows belong to which group)
  uint16_t n_weight_groups = w_group_ids.size();
  std::vector<arma::uvec> g_idx_vecs(n_weight_groups);
  std::vector<const arma::uvec*> g_idx_ptrs(n_weight_groups);

  // convert index of group to 0 based and arma::uvec type then store the pointers
  for (uint16_t j = 0; j < n_weight_groups; ++j) {
    g_idx_vecs[j] = Rcpp::as<arma::uvec>(w_group_ids[j]) - 1;
    g_idx_ptrs[j] = &g_idx_vecs[j];
  }

  // store position of Edesign columns in the design matrix. it is the last Edesign.n_cols of the design matrix
  arma::uvec Edesign_loc = arma::regspace<arma::uvec>(design.n_cols - Edesign.n_cols, design.n_cols - 1);

  // init ellipse slice params
  uint32_t df = basis.n_cols;
  uint32_t n_exposures = basis.n_rows;
  arma::rowvec nu(df);
  double threshold, eta, eta_max, eta_min;

  // init w params
  arma::rowvec theta_prop(df);
  arma::vec weighted_exposure(n_exposures);

  // init Edesign_subsets, which is an Edesign matrix for each group
  std::vector<arma::mat> Edesign_subsets(n_weight_groups);
  for (uint16_t j = 0; j < n_weight_groups; ++j) {
    Edesign_subsets[j] = Edesign.rows(*g_idx_ptrs[j]);
  }

  // init storage variables
  // 3D array groups * times * nits
  arma::cube w_keep(n_weight_groups, n_exposures, nits);
  // iters in columns. Save nits t() calls
  arma::mat regcoef_keep(n_regcoef, nits);
  arma::vec sigma_keep(nits);
  arma::mat ll_all_keep(n, nits);
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
      ll = arma::sum(dnorm_cpp(y.elem(*g_idx_ptrs[j]), design.rows(*g_idx_ptrs[j]), regcoef, inv_sigma, constant));
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

        weighted_exposure = exposure.rows(*g_idx_ptrs[j]) * w.row(j).t();
        design.submat(*g_idx_ptrs[j], Edesign_loc) = Edesign_subsets[j].each_col() % weighted_exposure;

        ll = arma::sum(dnorm_cpp(y.elem(*g_idx_ptrs[j]), design.rows(*g_idx_ptrs[j]), regcoef, inv_sigma, constant));

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

    regcoef_keep.col(i) = regcoef;
    sigma_keep(i) = sigma;

    if (REmodel) {
      REprec_keep(i) = REprec;
    }

    ll_all_keep.col(i) = dnorm_cpp(y, design, regcoef, inv_sigma, constant);
  }

  Rcpp::List result = Rcpp::List::create(
    Rcpp::Named("w_keep") = w_keep,
    Rcpp::Named("regcoef_keep") = regcoef_keep.t(),
    Rcpp::Named("sigma_keep") = sigma_keep,
    Rcpp::Named("ll_all_keep") = ll_all_keep
  );

  if (REmodel) {
    result["REprec_keep"] = REprec_keep;
  }

  return result;
}
