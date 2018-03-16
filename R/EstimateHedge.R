#' EstimateHedge
#'
#' Calculate optimal hedge ratio coefficients for a set of instruments
#' with the constraint that the sum of the coefficients equals 1
#'
#' @param target_var A data frame of dimension t x 1 containing the target
#' variable to be hedged.
#' @param hedge_vars A data frame of dimension t x k containing k variables
#' or istruments to be used within the hedge portfolio.
#'
#' @return A list object containing the constrained hedge ratios, the
#' unconstrained hedge ratios, the r-squared of both the constrained and
#' unconstrained solutions, as well as the efficiency loss (pct reduction in Rsq)
#' from the edge constraint.

EstimateHedge <- function( target_var, hedge_vars) {
  X = matrix(unlist(hedge_vars),ncol=ncol(hedge_vars),byrow=F)
  Y = matrix(target_var,ncol=1)
  Rinv = solve(chol(t(X) %*% X))
  C = cbind(rep(1,ncol(hedge_vars)),diag(ncol(hedge_vars)))
  b = c(1,rep(0,ncol(hedge_vars)))
  d = t(Y) %*% X
  soln = quadprog::solve.QP(Dmat = Rinv, factorized = TRUE, dvec = d, Amat = C,
                  bvec = b, meq = 1)
  rsq_constr = sum((X%*%soln$solution - mean(Y))^2)/sum((Y - mean(Y))^2)
  rsq_unconstr = sum((X%*%soln$unconstrained.solution - mean(Y))^2)/sum((Y - mean(Y))^2)
  loss = -(rsq_unconstr - rsq_constr)/rsq_unconstr
  result = list(constr_hedge_ratio=soln$solution, constr_rsq=rsq_constr,
                unconstr_hedge_ratio=soln$unconstrained.solution,
                unconstr_rsq=rsq_unconstr, efficiency_loss=loss)
  return(result)
}
