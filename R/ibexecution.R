
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
ib_send_order <- function(context, symbol, quantity,
                          action_type=c('BUY','SELL','SSHORT'),
                          order_type=c('MKT','MKTCLS','LMT','LMTCLS','STP','STPLMT','REL'),
                          limit_price=0,
                          aux_price=0,
                          parent_id=0,
                          time_in_force=c('DAY','GTC','IOC','GTD'),
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
  tws_order <- twsOrder(id,
                        action=action,
                        totalQuantity=quantity,
                        orderType=order,
                        lmtPrice=round(limit_price,2),
                        auxPrice=round(aux_price, 2),
                        parentId=parent_id,
                        tif=time_in_force,
                        transmit=transmit)

  #What about besides STK types?
  order_id <- placeOrder(context$get_connection(), twsSTK(symbol), tws_order)
  #twsDisconnect(context$get_connection())
  return(order_id)
}


#'
#' @export
#'
ib_buy_market_order <- function(context, symbol, quantity, order_type, time_in_force='GTC') {
  ib_send_order(context, symbol, quantity, 'BUY', 'MKT', time_in_force=time_in_force)
}

#'
#' @export
#'
ib_buy_limit_order <- function(context, symbol, quantity, order_type, limit_price=0, time_in_force='GTC') {
  ib_send_order(context, symbol, quantity, 'BUY', 'LMT', limit_price=limit_price, time_in_force=time_in_force)
}

#'
#' @export
#'
ib_sell_market_order <- function(context, symbol, quantity, order_type, time_in_force='GTC') {
  ib_send_order(context, symbol, quantity, 'SELL', 'MKT', time_in_force=time_in_force)
}

#'
#' @export
#'
ib_sell_limit_order <- function(context, symbol, quantity, order_type, limit_price=0, time_in_force='GTC') {
  ib_send_order(context, symbol, quantity, 'SELL', 'LMT', limit_price=limit_price, time_in_force=time_in_force)
}

#'
#' @export
#'
ib_stop_market_order <- function(context, symbol, quantity, time_in_force='GTC', parent_order_id=0, stop_price=0) {
  stopifnot(parent_order_id!=0)
  ib_send_order(context, symbol, quantity, 'SELL', order_type='STP', limit_price=limit_price,
                time_in_force=time_in_force, parent_id=parent_order_id, aux_price=stop_price)
}


#'
#' @export
#'
ib_stop_limit_order <- function(context, symbol, quantity, limit_price=0, time_in_force='GTC', parent_order_id=0, stop_price=0) {
  stopifnot(parent_order_id!=0)
  ib_send_order(context, symbol, quantity, 'SELL', order_type='STPLMT', limit_price=limit_price,
                time_in_force=time_in_force, parent_id=parent_order_id, aux_price=stop_price)
}


#'
#' @export
#'
ib_stop_adjustable_order <- function(context, symbol, quantity, limit_price=0, time_in_force='GTC', parent_order_id=0, stop_price=0) {
  stopifnot(parent_order_id!=0)
  #Set stop
  ib_send_order(context, symbol, quantity, 'SELL', order_type='STP', time_in_force=time_in_force, parent_id=parent_order_id, aux_price=stop_price)
  #add adjustment
  ib_send_order(context, symbol, quantity, 'SELL', order_type='TRAIL', limit_price=limit_price,
                time_in_force=time_in_force, parent_id=parent_order_id, aux_price=stop_price)
}

#'
#' @export
#'
ib_cancel_order <- function(context, order_id) {
  cancelOrder(context$get_connection(), order_id)
  #twsDisconnect(context$get_connection())
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
  #twsDisconnect(context$get_connection())
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


