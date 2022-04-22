#' fetch_3months_return_for_all
#' @param key API key 
#' @importFrom dplyr rename
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @export
#' @return kurs data frame

fetch_3months_return_for_all<-function(key=Sys.getenv("BORSDATA_KEY")){
  url <- paste0("https://apiservice.borsdata.se/v1/instruments/kpis/151/3month/return?authKey=",key)
  getdata<-httr::GET(url)
  data_json <- httr::content(getdata, type="text", encoding = "UTF-8")
  df_all <- jsonlite::fromJSON(data_json,)
  kurs<-df_all$values |>    dplyr::rename("insId"=i, "month3"=n)
  
  return(kurs)
}