#----Stop points
#(BuyPrice * Shares) - Loss = (BuyPrice * Shares) * (1 - pct)
#'
#' @export
#'
num_shares <- function(price, total_spent) {
  floor(total_spent / price)
}


#'
#' @export
#'
max_loss_stop_price <- function(price, shares, max_loss) {
  X <- price * shares
  pct <- 1 - (max_loss/X)
  return(max(0, price * pct))
}



