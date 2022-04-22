#' fetch_date_price_for_all
#' @param key API key 
#' @param DATE date
#' @importFrom dplyr  mutate left_join rename
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @export
#' @return kurs data frame

fetch_date_price_for_all<-function(DATE="2022-04-05", key=Sys.getenv("BORSDATA_KEY")){
  url <- paste0("https://apiservice.borsdata.se/v1/instruments/stockprices/date?authKey=",key,"&date=",DATE)
  getdata<-httr::GET(url)
  data_json <- httr::content(getdata, type="text", encoding = "UTF-8")
  df_all <- jsonlite::fromJSON(data_json,)
  kurs<-df_all$stockPricesList |>  
    dplyr::rename("insId"=i)
  
  return(kurs)
}