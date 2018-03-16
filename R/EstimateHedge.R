# EstimateHedge
#
# Estimates a hedge ratio for a target variable using a set of hedge variables

EstimateHedge <- function( target_var, hedge_vars) {
  # Calculate optimal hedge ratio coefficients for a set of instruments
  # with the constraint that the sum of the coefficients equals 1
  #
  # Args:
  #  target_var: A data frame of dimension tx1 containing the target variable
  #              to be replicated
  #  hedge_vars: A data frame of dimension txk containing k variables or
  #              instruments to be used as the hedge portfolio
  #
  # Returns:
  #  A list object containing the constrained hedge ratios, the unconstrained
  #  hedge ratios, the r-squared of both the constrained and unconstrained
  #  solutions, as well as the efficiency loss (pct reduction in Rsq) from the
  #  hedge constraint
  #
  # Requires: quadprog

  require(quadprog)
  X = matrix(unlist(hedge_vars),ncol=ncol(hedge_vars),byrow=F)
  Y = matrix(target_var,ncol=1)
  Rinv = solve(chol(t(X) %*% X))
  C = cbind(rep(1,ncol(hedge_vars)),diag(ncol(hedge_vars)))
  b = c(1,rep(0,ncol(hedge_vars)))
  d = t(Y) %*% X
  soln = solve.QP(Dmat = Rinv, factorized = TRUE, dvec = d, Amat = C,
                  bvec = b, meq = 1)
  rsq_constr = sum((X%*%soln$solution - mean(Y))^2)/sum((Y - mean(Y))^2)
  rsq_unconstr = sum((X%*%soln$unconstrained.solution - mean(Y))^2)/sum((Y - mean(Y))^2)
  loss = -(rsq_unconstr - rsq_constr)/rsq_unconstr
  result = list(constr_hedge_ratio=soln$solution, constr_rsq=rsq_constr,
                unconstr_hedge_ratio=soln$unconstrained.solution,
                unconstr_rsq=rsq_unconstr, efficiency_loss=loss)
  return(result)
}
