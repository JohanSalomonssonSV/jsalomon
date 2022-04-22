#' fetch_last_price_for_all
#' @param key API key 
#' @importFrom dplyr filter rename
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @export
#' @return kurs data frame
fetch_last_price_for_all<-function(key=Sys.getenv("BORSDATA_KEY")){
  url <- paste0("https://apiservice.borsdata.se/v1/instruments/stockprices/last?authKey=",key)
  getdata<-httr::GET(url)
  data_json <- httr::content(getdata, type="text", encoding = "UTF-8")
  df_all <- jsonlite::fromJSON(data_json,)
  kurs<-df_all$stockPricesList |>  dplyr::filter(d==max(d)) |>   dplyr::rename("insId"=i)
  
  return(kurs)
}