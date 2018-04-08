
debug <- function(msg) {
  print(msg)
}


#'
#' @export
#'
create_context <- function(twsport=4002) {
  if(exists('context', mode="environment")) {
    stop('**context already exists')
  }

  context <- new.env()
  context$twsport <- twsport
  context$twsconn <- NULL
  context$get_connection <- function() {
    if(is.null(context$twsconn)) {
      debug('NULL connection. Opening new TWS connection')
      context$twsconn <- twsConnect(port=context$twsport)
    }
    if(!isConnected(context$twsconn)) {
      debug('Disconnected. Opening new TWS connection')
      context$twsconn <- twsConnect(port=context$twsport)
    }
    return(context$twsconn)
  }

  return(context)
}



#'
#' @export
#'
is_valid_barsize <- function(bar_size) {
  validBarSize <-
    c("1 secs", "5 secs", "15 secs", "30 secs",
      "1 min", "2 mins", "3 mins", "5 mins", "15 mins", "30 mins",
      "1 hour", "1 day", "1 week", "1 month", "3 months", "1 year")
  bar_size %in% validBarSize
}

bar_size_to_secs <- function(bar_size) {
  if(!is_valid_barsize(bar_size)) {
    stop('Invalid bar size')
  }
  switch(bar_size,
         "1 secs"=1,"5 secs"=5, "15 secs"=15, "30 secs"=30,
         "1 min"=60, "2 mins"=120, "3 mins"=180, "5 mins"=300, "15 mins"=900, "30 mins"=1800,
         "1 hour"=3600, "1 day"=86400, "1 week"=604800, "1 month"=2419200, "3 months"=7257600, "1 year"=29030400)
}




#'
#' @export
#'
ib_account_details <- function(context) {
  reqAccountUpdates(context$get_connection())
}

#'
#' @export
#'
#'
ib_list_executions <- function(context, symbol='', secType='', exchange='',side='') {
  ex_filter <- twsExecutionFilter(symbol=symbol, secType=secType, exchange=exchange, side=side)
  execs <- reqExecOrders(context$get_connection(), reqId='0', ex_filter, verbose=TRUE)

  rr <- lapply(execs, function(exec) {
    contract <- exec$contract
    execution <- exec$execution
    tibble(symbol=contract$symbol,
           orderId=execution$orderId,
           time=lubridate::ymd_hms(execution$time),
           action=execution$side,
           shares=execution$shares,
           price=execution$price)
  })

  bind_rows(rr) %>%
    arrange(time)
}


#'
#' @export
#'
snapShot <- function (twsCon, eWrapper, timestamp, file, playback = 1, ...) {
  if (missing(eWrapper)) {
    eWrapper <- eWrapper()
  }

  names(eWrapper$.Data$data) <- eWrapper$.Data$symbols
  con <- twsCon[[1]]
  if (inherits(twsCon, "twsPlayback")) {
    sys.time <- NULL
    while (TRUE) {
      if (!is.null(timestamp)) {
        last.time <- sys.time
        sys.time <- as.POSIXct(strptime(paste(readBin(con, character(), 2), collapse = " "), timestamp))
        if (!is.null(last.time)) {
          Sys.sleep((sys.time - last.time) * playback)
        }
        curMsg <- .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE))
        if (length(curMsg) < 1) {
          next
        }
        processMsg(curMsg, con, eWrapper, format(sys.time, timestamp), file, ...)
      }
      else {
        curMsg <- readBin(con, character(), 1)
        if (length(curMsg) < 1)
          next
        processMsg(curMsg, con, eWrapper, timestamp, file, ...)
        if (curMsg == .twsIncomingMSG$REAL_TIME_BARS)
          Sys.sleep(5 * playback)
      }
    }
  }
  else {
    while (TRUE) {
      socketSelect(list(con), FALSE, NULL)
      curMsg <- .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE))
      if (!is.null(timestamp)) {
        processMsg(curMsg, con, eWrapper, format(Sys.time(), timestamp), file, ...)
      }
      else {
        processMsg(curMsg, con, eWrapper, timestamp, file, ...)
      }
      if (!any(sapply(eWrapper$.Data$data, is.na))) {
        return(do.call(rbind, lapply(eWrapper$.Data$data, as.data.frame)))
      } else {
        print(eWrapper$.Data$data)
      }
    }
  }
}


