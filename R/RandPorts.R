RandPorts <- function( num_ports , num_assets ) {
  # Returns a matrix of num_ports randomly generated portfolio weights that are
  # non-negative and sum to 1
  #
  # Args:
  #   num_ports: scalar input for the number of portfolios to be generated
  #   num_assets: scalar input for the number of securities in the portfolio
  #
  # Returns:
  #  a numeric matrix of dimension num_assets x num_ports
  #
  # Requires:
  #  n/a

  wgts = apply(rbind(rep(0,num_ports),
                     apply(array(runif(num_ports*(num_assets-1)),
                                 dim=c((num_assets-1),num_ports)),2,sort),
                     rep(1, num_ports)),2,diff)
  return(wgts)
}
