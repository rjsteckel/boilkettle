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


ib_send_order <- function(context, symbol, quantity, limit_price=0,
                       action_type=c('BUY','SELL','SSHORT'),
                       order_type=c('MKT','MKTCLS','LMT','LMTCLS','STP','STPLMT','REL'),
                       time_in_force=c('DAY','GTC','IOC','GTD'),
                       aux_price=0,
                       transmit=TRUE) {

  action <- match.arg(action_type)
  order <- match.arg(order_type)
  stopifnot(quantity > 0)

  if(order %in% c('LMT', 'STPLMT', 'REL')) {
    stopifnot(limit_price > 0)
  }

  if(order %in% c('STP')) {
    stopifnot(aux_price > 0)
  }

  id = as.numeric(reqIds(context$get_connection()))
  tws_order <- twsOrder(id, orderType=order,
                 lmtPrice=limit_price,
                 auxPrice=aux_price,
                 action=action,
                 totalQuantity=quantity,
                 tif=time_in_force,
                 transmit=transmit)

  order_id <- placeOrder(context$get_connection(), twsSTK(symbol), tws_order)
  return(order_id)
}


#'
#' @export
#'
ib_buy_order <- function(context, symbol, quantity, order_type, time_in_force='GTC', limit_price=0) {
  ib_send_order(context, symbol, quantity, limit_price, 'BUY', order_type, time_in_force)
}

#'
#' @export
#'
ib_sell_order <- function(context, symbol, quantity, order_type, time_in_force='GTC', limit_price=0) {
  ib_send_order(context, symbol, quantity, limit_price, 'SELL', order_type, time_in_force)
}

#'
#' @export
#'
ib_stop_order <- function(context, parent_order_id, quantity, order_type=c('STP','STPLMT'), time_in_force='GTC', stop_price=0, limit_price=0) {
  order <- match.arg(order_type)
  if(order=='STP') {
    stopifnot(stop_price > 0)
  }
  if(order=='STPLMT') {
    stopifnot(limit_price > 0)
  }

  ib_send_order(context, symbol, quantity, limit_price, 'SELL', order, time_in_force, aux_price=stop_price)
}

#'
#' @export
#'
ib_cancel_order <- function(context, order_id) {
  cancelOrder(context$get_connection(), order_id)
}


#'
#' @export
#'
ib_sell_short_order <- function(context, symbol, quantity, time_in_force, order, limit_price=0) {
  ib_send_order(context, symbol, quantity, limit_price, 'SSHORT', order, time_in_force)
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





#'
#' @export
#'
#'
handle_exits <- function(symbols, exit_strategy, bar_size='5 mins', look_back='1 W', verbose=TRUE) {
  if(!is_valid_barsize(bar_size)) {
    stop(paste('Invalid bar size:', bar_size))
  }

  if(!exists(as.character(substitute(exit_strategy)), mode='function')) {
    stop('Exit method does not exist')
  }

  positions_series <- list()
  for(symbol in symbols) {
    print(paste('Loading history for', symbol))
    series <- ib_historical(context, symbol, barSize=bar_size, duration=look_back)
    positions_series[[symbol]] <- series %>%
      select(datetime, open, high, low, close, volume, adjusted, count)
  }

  while(TRUE) {
    for(symbol in symbols) {
      tryCatch({
        #TODO: update with market data?
        series <- ib_historical(context, symbol, barSize=bar_size, duration='1 D')
        N <- nrow(series)
        last_bar <- series[N,]
        positions_series[[symbol]] <- if(last_bar$datetime > last(positions_series[[symbol]]$datetime)) {
          bind_rows(positions_series[[symbol]], last_bar)
        } else {
          if(verbose) {
            print(paste(symbol, 'No new data'))
          }
          positions_series[[symbol]]
        }

        position <- exit_strategy(positions_series[[symbol]])
        if(position==0) {
          print('exit')
        }

      }, error=function(e) {
        print(e)
      }, warning=function(w) {
        print(w)
      })
    }

    secs <- bar_size_to_secs(bar_size)
    print(paste('Sleeping for', secs))
    Sys.sleep(secs)
  }
}
