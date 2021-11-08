#' fetch_instruments
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @param key API key
#' @export
#' @return df data frame

fetch_instruments<-function(key=key){
  getdata<-httr::GET(url=paste0("https://apiservice.borsdata.se/v1/instruments?authKey=", key))
  data_json <- httr::content(getdata, type="text", encoding = "UTF-8")
  df <- jsonlite::fromJSON(data_json,)
  df <- df$instruments
  return(df)
}