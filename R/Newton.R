# Newton
#
# Find the root of a polynomial function

Newton <- function( f, h=0.01, tol=1e-12, x0=1, maxiter ) {
  # Find the root of a polynomial function
  #
  # Args:
  #   f: a function to be evaluated
  #   h: the step size (default 0.01)
  #   tol: the error tolerance for function approximation (default 1e-12)
  #   x0: initial value
  #   maxiter: maximum number of iterations before exit
  #
  # Returns:
  #   list containing iteration values, the estimated root, and the number of
  #   iterations used to estimate the root
  #
  # Requires:

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
