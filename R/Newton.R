#' Newton()
#'
#' Find the root of a polynomial function.
#'
#' @param f A function to be evaluated.
#' @param h Optional parameter. The step size. Default 0.01.
#' @param tol Optional parameter. The error tolerance for the function
#' appriximation. Default 1e-12.
#' @param x0 Optional parameter. Initial value. Default 1.
#' @param maxiter Maximum number of iterations before exit.
#'
#' @return List containing iteration values, the estimated root, and the
#' number of terations used to estimate the root.

Newton <- function( f, h=0.01, tol=1e-12, x0=1, maxiter ) {
  i = 1
  p = numeric(maxiter)
  while (i < maxiter) {
    df_dx = (f(x0+h)-f(x0))/h
    x1 = (x0 - f(x0)/df_dx)
    p[i] = x1
    i = i + 1
    if (abs(x1-x0) < tol) break
    x0 = x1
  }
  return(list(vals=p[1:(i-1)],res=p[(i-1)],num_iter=i))
}
