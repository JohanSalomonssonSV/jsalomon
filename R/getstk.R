#' getstk
#' @param ticker ticker
#' @param start_date start_date
#' @param end_date end_date
#' @importFrom lubridate today 
#' @importFrom tidyquant tq_get
#' @export
#' @return df
getstk<-function(ticker, start_date="2015-01-01", end_date=lubridate::today()){
  df <-ticker |>
    tidyquant::tq_get( from = start_date, to = end_date#, periodicity = "weekly"
    )
  
  return(df)
  
}
