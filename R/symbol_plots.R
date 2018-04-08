
#'
#' @export
#'
plot_equity_candles <- function(prices) {
  prices %>%
        tk_xts(silent=TRUE) %>%
        chartSeries(TA="addVo();addBBands();addRSI()")
}

#'
#' @export
#'
plot_commodity_candles <- function(prices) {
  prices %>%
    tk_xts(silent=TRUE) %>%
    chartSeries(TA="addVo();addBBands();addCCI()")
}

# plot_dynamic <- function() {
#   prices %>%
#     tk_xts %>%
#     dygraph(series, main = "") %>%
#     dyRangeSelector(dateWindow = c("2010-01-18", "2017-10-01"))
#
#   for(sell in as.list(sells)) {
#     pp <- dyShading(pp, from=sell, to=sell+days(3), color="red")
#   }
#   for(buy in as.list(buys)) {
#     pp <- dyShading(pp, from=buy, to=buy+days(3), color="green")
#   }
#   print(pp)
# }

