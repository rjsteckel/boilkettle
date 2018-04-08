

#'
#' @export
#'
ib_list_open_orders <- function(context) {
  oo <- reqOpenOrderContracts(context$get_connection())

  order_list <- list()
  for(i in 1:length(oo)) {
    contract <- oo[[i]]$contract
    order <- oo[[i]]$order

    o <- as.tibble(data.frame(order_id=order$orderId,
                              parent_id=order$parentId,
                              symbol=contract$symbol,
                              sectype=contract$sectype,
                              exchange=contract$exch,
                              action=order$action,
                              quantity=order$totalQuantity,
                              order_type=order$orderType,
                              limit_price=order$lmtPrice,
                              stop_price=order$auxPrice,
                              time_in_force=order$tif))

    order_list[[length(order_list)+1]] <- o
  }

  return(bind_rows(order_list))
}


#'
#' @export
#'
ib_contract_details <- function(context, symbol) {
  reqContractDetails(context$get_connection(), twsSTK(symbol))
}



#'
#' @export
#'
ib_send_order <- function(context, contract, quantity,
                          action_type=c('BUY','SELL'),
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

  if(!is.twsContract(contract)) {
    stop("invalid contract")
  }

  #What about besides STK types?
  order_id <- placeOrder(context$get_connection(), contract, tws_order)
  #twsDisconnect(context$get_connection())
  return(order_id)
}


#---------------Equities---------------

#'
#' @export
#'
ib_equity_buy_market_order <- function(context, symbol, quantity, time_in_force='GTC') {
  contract <- twsSTK(symbol)
  ib_send_order(context, contract, quantity, 'BUY', 'MKT', time_in_force=time_in_force)
}

#'
#' @export
#'
ib_equity_buy_limit_order <- function(context, symbol, quantity, limit_price=0, time_in_force='GTC') {
  contract <- twsSTK(symbol)
  ib_send_order(context, contract, quantity, 'BUY', 'LMT', limit_price=limit_price, time_in_force=time_in_force)
}

#'
#' @export
#'
ib_equity_sell_market_order <- function(context, symbol, quantity, time_in_force='GTC') {
  contract <- twsSTK(symbol)
  ib_send_order(context, contract, quantity, 'SELL', 'MKT', time_in_force=time_in_force)
}

#'
#' @export
#'
ib_equity_sell_limit_order <- function(context, symbol, quantity, limit_price=0, time_in_force='GTC') {
  contract <- twsSTK(symbol)
  ib_send_order(context, contract, quantity, 'SELL', 'LMT', limit_price=limit_price, time_in_force=time_in_force)
}

#'
#' @export
#'
ib_equity_stop_market_order <- function(context, symbol, quantity, time_in_force='GTC', parent_order_id=0, stop_price=0) {
  stopifnot(parent_order_id!=0)
  contract <- twsSTK(symbol)
  ib_send_order(context, contract, quantity, 'SELL', 'STP', time_in_force=time_in_force, parent_id=parent_order_id, aux_price=stop_price)
}


#'
#' @export
#'
ib_equity_stop_limit_order <- function(context, symbol, quantity, limit_price=0, time_in_force='GTC', parent_order_id=0, stop_price=0) {
  stopifnot(parent_order_id!=0)
  contract <- twsSTK(symbol)
  ib_send_order(context, contract, quantity, 'SELL', order_type='STPLMT', limit_price=limit_price,
                time_in_force=time_in_force, parent_id=parent_order_id, aux_price=stop_price)
}


#---------------Options---------------

#'
#' @export
#'
ib_option_put_order <- function(context, symbol, quantity, strike, time_in_force='GTC') {
  contract <- twsOption(symbol, strike=strike, right='PUT', exch='IDEALPRO')
  ib_send_order(context, contract, quantity, 'BUY', 'MKT', time_in_force=time_in_force)
}




#'
#' @export
#'
ib_cancel_order <- function(context, order_id) {
  cancelOrder(context$get_connection(), order_id)
  #twsDisconnect(context$get_connection())
}

