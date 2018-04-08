
screener <- function() {
  active <- high_activity()
  safe_symbols <- gsub('\\.', '-', active$symbol)
  print(knitr::kable(active[order(-pct_change)]))
  krs <- kelly_ratios(safe_symbols)
  active <- merge(active, krs, by='symbol', all.x=TRUE)
  active_data <- symbol_data_points(active$symbol)
  active <- merge(active, active_data, by='symbol', all.x=TRUE)
  print(knitr::kable(active[order(-pct_change)]))

  dd <- data.table(symbol=si$ticker,
             name=si$name,
             exchange=si$stock_exchange,
             sic=ifelse(is.null(si$sic), '', si$sic),
             employees=ifelse(is.null(si$employees), '', si$employees),
             entity_legal_form=ifelse(is.null(si$entity_legal_form), '', si$entity_legal_form),
             latest_filing_date=ifelse(is.null(si$latest_filing_date), '', si$latest_filing_date),
             entity_status=ifelse(is.null(si$entity_status), '', si$entity_status),
             sector=ifelse(is.null(si$sector), '', si$sector),
             industry_category=ifelse(is.null(si$industry_category), '', si$industry_category),
             industry_group=ifelse(is.null(si$industry_group), '', si$industry_group),
             description=si$short_description)

  sec <- lapply(si$securities, unlist)

  print(dd)
  print(sec)
}
