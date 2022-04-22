#' fetch_volatility_for_all
#' @param key API key 
#' @param days number of days
#' @importFrom dplyr rename
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @export
#' @return kurs data frame

fetch_volatility_for_all<-function(days=30,key=Sys.getenv("BORSDATA_KEY")){
  
  url <- paste0("https://apiservice.borsdata.se/v1/instruments/kpis/311/",days,"day/default?authKey=",key)
  
  getdata<-httr::GET(url)
  data_json <- httr::content(getdata, type="text", encoding = "UTF-8")
  df_all <- jsonlite::fromJSON(data_json,)
  kurs<-df_all$values |>    dplyr::rename("insId"=i, "volatility"=n)
  
  return(kurs)
}