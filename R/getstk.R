#' getstk
#' @param ticker ticker
#' @param start_date start_date
#' @importFrom lubridate today 
#' @importFrom tidyquant tq_get
#' @export
#' @return df
getstk<-function(ticker, start_date="2015-01-01"){
  df <-ticker |>
    tidyquant::tq_get( from = start_date, to = lubridate::today()#, periodicity = "weekly"
    )
  
  return(df)
  
}
