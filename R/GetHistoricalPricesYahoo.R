#' GetHistoricalPricesYahoo()
#'
#' A helper function to automate the download of a matrix of historical EOD
#' prices from Yahoo!Finance. Makes use of the get.hist.quote() function from
#' the tseries package.
#'
#' @param tickers Vector of tickers to be downloaded.
#' @param start_date Date for start of time series.
#' @param end_date Date for end of time series.
#'
#' @return Data frame containing the dates in column 1 and the prices for
#' each ticker in the remaining columns.
#'
#' @export

GetHistoricalPricesYahoo <- function( tickers , start_date , end_date ) {
  if (length(tickers) > 1) {
    data <- tseries::get.hist.quote(instrument=tickers[1],start=start_date,
                                    end=end_date,
                                    quote=c("Adjusted"),
                                    provider="yahoo",
                                    retclass="zoo",
                                    quiet=T)
    for (i in 2:length(tickers)) {
      hist <- tseries::get.hist.quote(instrument=tickers[i],
                                      start=start_date,
                                      end=end_date,
                                      quote=c("Adjusted"),
                                      provider="yahoo",
                                      retclass="zoo",
                                      quiet=T)
      data <- merge(data,hist)}
    colnames(data) <- tickers
  } else {
    data <- tseries::get.hist.quote(instrument=tickers[1],start=start_date,
                                    end=end_date,
                                    quote=c("Adjusted"),
                                    provider="yahoo",
                                    retclass="zoo",
                                    quiet=T)
    colnames(data) <- tickers
  }
  return(data)
}
