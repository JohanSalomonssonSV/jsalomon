#' perf_table1
#' @param country country sweden america
#' @param exchange exchange like OMXSTO
#' @importFrom httr POST add_headers content
#' @importFrom jsonlite fromJSON
#' @importFrom data.table data.table rbindlist
#' @export
#' @return z tibble
#' 
perf_table1<-function(country="sweden", exchange="OMXSTO"){
  
  
  # Big thanks to https://github.com/misrori who I borrowed most of the code from
  get_tradingview_data_from_json_string <- function(my_json_string) {
    headers = c(
      `authority` = 'scanner.tradingview.com',
      `accept` = 'text/plain, */*; q=0.01',
      `origin` = 'https://www.tradingview.com',
      `user-agent` = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.108 Safari/537.36',
      `content-type` = 'application/x-www-form-urlencoded; charset=UTF-8',
      `sec-fetch-site` = 'same-site',
      `sec-fetch-mode` = 'cors',
      `referer` = 'https://www.tradingview.com/',
      `accept-encoding` = 'gzip, deflate, br',
      `accept-language` = 'hu-HU,hu;q=0.9,en-US;q=0.8,en;q=0.7'
    )
    
    
    res <- httr::POST(url = paste0('https://scanner.tradingview.com/',country,'/scan'), httr::add_headers(.headers=headers), body = my_json_string)
    
    t <- jsonlite::fromJSON(httr::content(res, 'text'))
    my_df_data <-
      data.table::rbindlist(lapply(t$data$d, function(x){
        data.frame(t(data.frame(x)), stringsAsFactors = F)
      }))
    
    my_names <- strsplit(strsplit( strsplit(gsub('\"', '', my_json_string, fixed = T), 'columns:[', fixed = T)[[1]][2], '],sort',  fixed = T )[[1]][1], ',', fixed = T)[[1]]
    
    names(my_df_data) <- my_names
    final_data <- cbind( data.table::data.table('exchange' = sapply(strsplit(t$data$s, ':'), '[[', 1)),  my_df_data)
    return(final_data)
  }
  
  get_performance_table <- function() {
    adat <- get_tradingview_data_from_json_string(paste0('{"filter":[{"left":"market_cap_basic","operation":"nempty"},{"left":"type","operation":"in_range","right":["stock","dr","fund"]},{"left":"subtype","operation":"in_range","right":["common","","etf","unit","mutual","money","reit","trust"]},{"left":"exchange","operation":"in_range","right":["',exchange,'"]}],"options":{"lang":"en"},"symbols":{"query":{"types":[]},"tickers":[]},"columns":["logoid","name","change|1","change|5","change|15","change|60","change|240","change","change_from_open","Perf.W","Perf.1M","Perf.3M","Perf.6M","Perf.YTD","Perf.Y","beta_1_year","SMA10","SMA20" ,"SMA50", "SMA200","EMA10","EMA20","EMA50","Value.Traded", "VWAP","ATR" ,"average_volume_10d_calc","average_volume_30d_calc","average_volume_60d_calc", "ebitda","ebitda_yoy_growth_fy","ebitda_qoq_growth_fq","ebitda_yoy_growth_fq","net_income_yoy_growth_fy","net_income_qoq_growth_fq","net_income_yoy_growth_fq","net_income_yoy_growth_ttm","earnings_per_share_forecast_next_fq","earnings_per_share_diluted_yoy_growth_ttm", "return_on_equity","after_tax_margin" ,"Mom","Recommend.All", "Volatility.D","RSI","number_of_employees","market_cap_basic","sector","industry","country","Low.6M","High.All","Low.All","High.1M","High.3M","High.6M", "price_52_week_high","price_earnings_ttm","close","open","volume","high","low","description","earnings_release_next_date","premarket_change_abs","premarket_change","premarket_volume","postmarket_change","name","type","subtype","update_mode","RSI","RSI[1]","pricescale","minmov","fractional","minmove2"],"sort":{"sortBy":"market_cap_basic","sortOrder":"desc"},"range":[0,6550]}'))
    return(adat)}
  
  z<-get_performance_table()
  
  z<-jsalomon::clean_TVdata(z)
  
  names(z)[3]<-"name"
  return(z)
  
}
#t<-perf_table1()