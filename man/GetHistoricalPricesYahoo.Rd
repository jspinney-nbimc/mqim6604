% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetHistoricalPricesYahoo.R
\name{GetHistoricalPricesYahoo}
\alias{GetHistoricalPricesYahoo}
\title{GetHistoricalPricesYahoo()}
\usage{
GetHistoricalPricesYahoo(tickers, start_date, end_date)
}
\arguments{
\item{tickers}{Vector of tickers to be downloaded.}

\item{start_date}{Date for start of time series.}

\item{end_date}{Date for end of time series.}
}
\value{
Data frame containing the dates in column 1 and the prices for
each ticker in the remaining columns.
}
\description{
A helper function to automate the download of a matrix of historical EOD
prices from Yahoo!Finance. Makes use of the get.hist.quote() function from
the tseries package.
}
