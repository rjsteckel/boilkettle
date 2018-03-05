
#'
#' @export
#'
#' @import dplyr
#' @import timetk
#'
ib_historical <- function(context, symbol, barSize='15 mins', duration='1 W') {
  if(!is_valid_barsize(barSize)) {
    stop('Invalid barsize')
  }

  data <- reqHistoricalData(context$get_connection(), twsSTK(symbol), barSize=barSize, duration=duration)
  tt <- tk_tbl(data, timetk_idx=TRUE)
  colnames(tt) <- c('datetime', 'open', 'high', 'low', 'close', 'volume', 'adjusted', 'has_gaps', 'count')
  tt <- tt %>% arrange(datetime)

  return(tt)
}

#'
#' @export
#'
ib_historical_fx <- function(context, currency, barSize='15 mins', duration='1 W') {
  if(!is_valid_barsize(barSize)) {
    stop('Invalid barsize')
  }

  twscurr <- twsCurrency(currency)
  twscurr$primary <- 'IDEALPRO'
  data <- reqHistoricalData(context$get_connection(), twscurr, barSize=barSize, duration=duration, whatToShow="ASK")
  tt <- data %>% tk_tbl()
  colnames(tt) <- c('datetime', 'open', 'high', 'low', 'close', 'volume', 'wap', 'has_gaps', 'count')
  tt <- tt %>% arrange(datetime)

  return(tt)
}


#'
#' @export
#'
#' @import dplyr
#' @import timetk
#'
ib_market_data <- function(context, symbol, exchange='SMART') {
  md <- reqMktData(context$get_connection(), twsSTK(symbol, exch=exchange), eventWrapper=eWrapper.data(1), CALLBACK=snapShot)
  tt <- as_tibble(md)
  return(tt)
}

#'
#' @export
#'
ib_position_symbols <- function(contex, type='STK') {
  acct <- reqAccountUpdates(context$get_connection())
  symbols <- lapply(acct[[2]], function(contract) { ifelse(contract$contract$sectype==type, contract$contract$symbol, NA) })
  symbols <- symbols[!sapply(symbols, is.na)]
  return(unlist(symbols))
}


#'
#' @export
#'
ib_position_contracts <- function(contex, type='ALL') {
  acct <- reqAccountUpdates(context$get_connection())
  contracts <- lapply(acct[[2]], function(contract) { contract$contract })
  if(type!='ALL') {
    met <- sapply(contracts, function(cntr) cntr$sectype==type)
    contracts <- contracts[met]
  }
  return(contracts)
}

