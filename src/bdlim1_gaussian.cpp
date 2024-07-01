#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
Rcpp::List bdlim1_gaussian_cpp(const arma::vec& y, arma::u64 nits, const arma::mat& design,
                               bool REmodel, double REprec, arma::u64 nRE, arma::u64 n_regcoef) {
    arma::u64 n = y.n_elem;
    double sigma = arma::stddev(y);
    arma::vec sigma_keep(nits, arma::fill::none);
    arma::vec pred_mean_model_scale(n, arma::fill::none);

    // Declare V outside the loop
    arma::mat V;

    // MCMC loop
    for (arma::u64 i = 0; i < nits; ++i) {
        // Update regression coefficients
        V = design.t() * design / (sigma * sigma);

        // Calculate diag_v inside the loop
        arma::vec diag_v(n_regcoef);
        if (REmodel) {
            diag_v.head(nRE).fill(REprec);
            diag_v.tail(n_regcoef - nRE).fill(1.0 / 100);
        } else {
            diag_v.fill(1.0 / 100);
        }

        // Update diagonal of V
        V.diag() += diag_v;

        // Store sigma for this iteration
        sigma_keep(i) = sigma;

        // Other calculations will go here
    }

    return Rcpp::List::create(
        Rcpp::Named("n") = n,
        Rcpp::Named("sigma") = sigma,
        Rcpp::Named("sigma_keep") = sigma_keep,
        Rcpp::Named("pred_mean_model_scale") = pred_mean_model_scale,
        Rcpp::Named("V") = V
    );
}
