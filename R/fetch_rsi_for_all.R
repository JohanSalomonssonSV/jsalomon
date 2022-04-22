#' fetch_rsi_for_all
#' @param key API key 
#' @param days number of days
#' @importFrom dplyr rename
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @export
#' @return kurs data frame

fetch_rsi_for_all<-function(days=3,key=Sys.getenv("BORSDATA_KEY")){
  # c(10,20,30,50,100)
  url <- paste0("https://apiservice.borsdata.se/v1/instruments/kpis/160/",days,"day/trend?authKey=",key)
  getdata<-httr::GET(url)
  data_json <- httr::content(getdata, type="text", encoding = "UTF-8")
  df_all <- jsonlite::fromJSON(data_json,)
  kurs<-df_all$values |>    dplyr::rename("insId"=i, "rsi"=n)
  
  return(kurs)
}