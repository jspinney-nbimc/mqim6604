#' RandPorts()
#'
#' Returns a matrix of num_ports randomly generated portfolio weights that are
#' non-negative and sum to 1.
#'
#' @param num_ports Scalar input number of desired portfolios.
#' @param num_assets Scalar input nummber of assets within each portfolio.
#'
#' @return A numeric matrix of dimension num_assets x num_ports containing the
#' random portfolio weights.
#'
#' @export

RandPorts <- function( num_ports , num_assets ) {
  wgts = apply(rbind(rep(0,num_ports),
                     apply(array(runif(num_ports*(num_assets-1)),
                                 dim=c((num_assets-1),num_ports)),2,sort),
                     rep(1, num_ports)),2,diff)
  return(wgts)
}
