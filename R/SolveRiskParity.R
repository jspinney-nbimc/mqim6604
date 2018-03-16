#' SolveRiskParity()
#'
#' Finds the risk parity portfolio for a given covariance matrix (defined as
#' an equal risk contribution portfolio where pctr_i = pctr_j for all i, j).
#' Uses "Algorithm 1" of Chaves et al (2012) "Efficient Algorithms for
#' Computing Risk Parity Portfolio Weights".
#'
#' @param sigma A variance-covariance matrix for assets in the investment universe.
#' @param err_tol Optional parameter. Tolerance for exiting while loop (may
#' experience convergence issues with large problems - if results do not
#' produce expected pctr_i = pctr_j forall i, j then test smaller error
#' err_tol (default = 0.00000001).
#'
#' @return A list item containing 'wts' (a vector of weights for the resulting
#' optimal ERC portfolio), 'mctr' (the marginal contributions to risk for each
#' asset class for the ERC portfolio), 'pctr' (the percent contributions to risk
#' for each asset class in the ERC portfolio), 'lambda' (optimal lambda).
#'
#' @export

SolveRiskParity <- function(sigma, err_tol=0.00000001) {

  # helper functions F & invert_j
  F <- function(y,V){
    n = length(y)
    wts = as.matrix(y[-n])
    lambda = y[n]
    rbind(V%*%wts-lambda/wts,sum(wts)-1)
  }

  invert_j <- function(y,V){
    # find inverse of j matrix
    n = length(y)
    wts = as.matrix(y[-n])
    lambda = y[n]
    J = rbind(cbind(V+lambda*diag(as.numeric(1/wts^2)),-1/wts),c(rep(1,p),0))
    solve(J)
  }

  # base data
  p = ncol(sigma)
  init_wts = matrix(rep(1/p, p))
  port_var = t(init_wts)%*%sigma%*%init_wts
  init_lambda = as.numeric(port_var/p)
  init_y = rbind(init_wts, init_lambda)

  # optimization routine (newton method of Chaves et al (2012))
  y = init_y
  epsilon = .0002
  while(epsilon >= err_tol) {
    a = F(y, sigma)
    b = invert_j(y, sigma)
    y_n = y - b %*% a
    epsilon = sqrt(sum(y_n - y)^2)
    y = y_n
  }

  # extract results
  wts_opt = matrix(y_n[1:p],ncol=1)
  lambda_opt = y_n[p+1]

  # check results
  c = sigma %*% wts_opt
  d = lambda_opt * 1/wts_opt

  # calculate mctr and pctr
  opt_port_sigma = sqrt(t(wts_opt) %*% sigma %*% wts_opt)
  marg_ctr_rsk = sigma %*% wts_opt/as.numeric(opt_port_sigma)
  pct_ctr_rsk = 100 * wts_opt * marg_ctr_rsk / as.numeric(opt_port_sigma)

  return(list(port_wgt=wts_opt, mctr=marg_ctr_rsk, pctr=pct_ctr_rsk, lambda=lambda_opt))
}
