db_connect <- function() {
  pg <- src_postgres(dbname='boilkettle', host='localhost',
                     user=Sys.getenv('DB_USERNAME'),
                     password=Sys.getenv('DB_PASSWORD'))
}

create_position <- function(symbol, position_type=c('ENTERING', 'LONG', 'SHORT', 'EXITING', 'CLOSED'),
                            quantity=0,
                            order_id=-1,
                            parent_order_id=-1,
                            limit_price=0.0,
                            initial_date=now(),
                            initial_price=0.0,
                            last_updated=now()) {

  position <- match.arg(position_type)
  positions_object <- data.table(symbol=symbol,position=position, quantity=quantity, order_id=order_id, parent_order_id=parent_order_id,
                                 limit_price=limit_price, initial_date=initial_date, initial_price=initial_price, last_updated=last_updated)
  return(positions_object)
}

#'
#' @export
#'
create_db <- function(refresh=FALSE) {
  pg <- db_connect()
  if(refresh) {
    current_tables <- DBI::dbListTables(pg$con)
    for(tb in current_tables) {
      print(paste('Dropping', tb))
      db_drop_table(pg$con, tb)
    }
  }

  positions_schema <- create_position('TEMP', 'ENTERING')
  # create table and populate to set column types
  copy_to(pg, positions_schema, 'positions', temporary=FALSE)
  clear_position('TEMP') #remove temp position after setting types

  dbDisconnect(pg$con)
}

#'
#' @export
#'
enter_position <- function(context, symbol, quantity, order_type,
                           position_type=c('LONG', 'SHORT'),
                           limit_price=NULL) {
  stopifnot(quantity > 0)

  order_id <- switch(match.arg(position_type),
                     LONG=ib_buy_order(context, symbol, quantity, order_type, limit_price=limit_price),
                     SHORT=ib_sell_short_order(context, symbol, quantity, order_type, limit_price=limit_price))

  newposition <- create_position(symbol, 'ENTERING', quantity=quantity, order_id=order_id, limit_price=limit_price)

  pg <- db_connect()
  db_insert_into(con=pg$con, table='positions', values=newposition)
  dbDisconnect(pg$con)

  return(newposition)
}


#'
#' @export
#'
exit_position <- function(context, symbol, quantity, time_in_force, order_type='MKT') {

  sell_order(context, symbol, quantity, time_in_force, order_type)

  newposition <- create_position(symbol, 'EXITING', quantity=quantity)

  pg <- db_connect()
  db_insert_into(con=pg$con, table='positions', values=newposition)
  dbDisconnect(pg$con)

  return(newposition)
}

#'
#' @export
#'
clear_position <- function(symbol) {

  query_string <- sprintf("delete from positions where symbol='%s'", symbol)

  pg <- db_connect()
  dbSendQuery(pg$con, query_string)
  dbDisconnect(pg$con)
}


#'
#' @export
#'
load_positions <- function() {
  pg <- db_connect()

  positions <- tbl(pg, 'positions') %>%
    collect()

  return(positions)
}


#'
#' @export
#'
load_position <- function(symbol) {
  pg <- db_connect()

  position <- tbl(pg, 'positions') %>%
    filter(symbol==symbol) %>%
    collect()

  return(position)
}


