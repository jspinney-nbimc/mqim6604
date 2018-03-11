# risk_parity.R finds the risk parity portfolio for a given covariance matrix
# (defined as an equal risk contribution portfolio where pctr_i = pctr_j
# forall i, j)
# uses algorithm of chaves et al (2012)
# jon spinney, original:2013, this version: 2018

SolveRiskParity <- function(sigma, err_tol=0.00000001) {
  # solves for the risk parity portfolio weights
  #
  # inputs:
  #   sigma: a variance-covariance matrix
  #   err_tol (default = 0.00000001): tolerance for exiting while loop (may
  #     experience convergence issues with large problems - if results do not
  #     produce expected pctr_i = pctr_j forall i, j then test smaller error
  #     tolerance)
  #
  # returns:
  #   wts: vector of weights for optimal portfolio
  #   mctr: marginal contributions to risk for each asset class for optimal
  #     portfolio
  #   pctr: percent contributions to risk for each asset class for optimal
  #     portfolio
  #   lambda: optimal lambda
  #
  # usage: PortSoln <- SolveRiskParity(covariancematrix)
  #   access elements of PortSoln with "$" operator
  #
  # notes: sensitive to inputs - please use sensible covariance matrix (semi-
  #   positive definite, perhaps use Bayesian Shrinkage methodology)

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
