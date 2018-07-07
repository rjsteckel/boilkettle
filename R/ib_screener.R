
#'
#'
#' @export
#'
ib_screener_top_pct_lose <- function(context) {
  #HIGH_OPT_VOLUME_PUT_CALL_RATIO, TOP_PERC_GAIN, HOT_BY_VOLUME, MOST_ACTIVE, HIGH_OPT_VOLUME_PUT_CALL_RATIO
  scan_code <- 'TOP_PERC_LOSE'
  scanner <- twsScannerSubscription(numberOfRows=100,
                                    instrument="STK",
                                    locationCode="STK.US.MAJOR",
                                    scanCode=scan_code,
                                    abovePrice=10,
                                    aboveVolume=0,
                                    marketCapAbove=1e8,
                                    scannerSettingPairs="Annual,true",
                                    stockTypeFilter="ALL")
  print(paste('Scan code', scan_code))
  scan_results <- reqScannerSubscription(context$get_connection(), scanner)

  return(scan_results)
}
