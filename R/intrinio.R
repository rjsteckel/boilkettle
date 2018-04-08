

#' https://api.intrinio.com/historical_data?identifier=$SIC.3500&item=totalrevenue&frequency=monthly&type=median
#' https://api.intrinio.com/owners?institutional=true
#' https://api.intrinio.com/indices?type=sic&query=mining
#' https://api.intrinio.com/securities
#'
#' http://docs.intrinio.com/tags/intrinio-public#screener
#'
#'
call_intrinio <- function(url) {
  username <- Sys.getenv('INTRINIO_USER')
  password <- Sys.getenv('INTRINIO_PASS')
  page <- httr::GET(url, httr::authenticate(username, password))
  results <- httr::content(page)
  if(length(results$errors) > 0) {
    print(results$errors)
  }
  return(results)
}

#'
#' @export
#'
symbol_historical_data <- function(symbol) {
  url <- paste0('https://api.intrinio.com/historical_data?identifier=', symbol, '&item=totalrevenue')
  results <- call_intrinio(url)

  print(paste('Results:', results$result_count))
  print(paste('API call credits:', results$api_call_credits))

  rr <- lapply(results$data, function(r) data.table(date=r$date, totalrevenue=r$value))
  rbindlist(rr)
}

#'
#' @export
#'
symbol_info <- function(symbol) {
  url <- paste0('https://api.intrinio.com/companies?ticker=', symbol)
  results <- call_intrinio(url)

  print(paste('Results:', results$result_count))
  print(paste('API call credits:', results$api_call_credits))

  lapply(results$securities, as.data.table)
  return(results)
}

#'
#' @export
#'
symbol_data_points <- function(symbols) {
  data_points <- c('weightedavedilutedsharesos', 'dilutedeps', 'marketcap', 'epsgrowth')
  url <- paste0('https://api.intrinio.com/data_point?identifier=', paste0(symbols, collapse=','), '&item=', paste0(data_points, collapse=','))
  results <- call_intrinio(url)

  print(paste('Results:', results$result_count))
  print(paste('API call credits:', results$api_call_credits))

  rr <- lapply(results$data, function(r) {
    data.table(symbol=r$identifier, variable=r$item, value=r$value)
  })

  return(dcast(rbindlist(rr), symbol ~ variable))
}

#'
#' @export
#'
high_activity <- function(min_open=10, min_volume=5000, min_pct_change=.05, max_days=1) {
  query <- sprintf('open_price~gt~%d,volume~gt~%d,percent_change~gt~%f,days_to_cover~lt~%d',
                   min_open, min_volume, min_pct_change, max_days)
  url <- paste0('https://api.intrinio.com/securities/search?conditions=', query)
  results <- call_intrinio(url)
  rr <- lapply(results$data, function(r) data.table(symbol=r$ticker,
                                                    open=r$open_price,
                                                    volume=r$volume,
                                                    pct_change=r$percent_change*100))
  return(rbindlist(rr))
}


#**Fee based
insider <- function(symbol) {
  query <- sprintf('identifier=%s', symbol)
  url <- paste0('https://api.intrinio.com/companies/insider_transactions?', query)
  results <- call_intrinio(url)
  rr <- lapply(results$data, function(r) data.table(symbol=r$ticker))
  return(rbindlist(rr))
}

