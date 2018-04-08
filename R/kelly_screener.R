#'
#' @export
#'
#' @import lubridate
#'
kelly_ratios <- function(symbols, start_date=today() - months(6)) {
  prices <- tq_get(symbols, from=start_date)
  returns <- prices %>%
    filter(complete.cases(.)) %>%
    group_by(symbol) %>%
    tq_mutate(select = adjusted,
              mutate_fun = periodReturn,
              period = "daily",
              type = "log",
              col_rename='returns')

  Rb <- "^GSPC" %>%
    tq_get(get  = "stock.prices",
           from = start_date) %>%
    tq_transmute(adjusted,
                 periodReturn,
                 period = "daily",
                 type='log',
                 col_rename = "Rb")

  RaRb <- left_join(returns, Rb, by = c("date" = "date"))

  kr <- RaRb %>%
    tq_performance(Ra=returns, Rb=Rb, performance_fun=KellyRatio) %>%
    arrange(desc(KellyRatio))

  return(kr)
}
