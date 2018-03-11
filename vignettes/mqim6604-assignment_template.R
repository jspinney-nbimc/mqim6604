## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- results='asis', echo=FALSE-----------------------------------------
cat("`` `r 2+2` ``")

## ----eval=FALSE----------------------------------------------------------
#  library(quantmod,quietly=TRUE)
#  getSymbols("^GSPC")
#  head(GSPC)

## ----message=FALSE,warning=FALSE-----------------------------------------
library(quantmod,quietly=TRUE)
getSymbols("^GSPC")
head(GSPC)

## ------------------------------------------------------------------------
library(quantmod)
getSymbols("^GSPC")
chart_Series(GSPC)

