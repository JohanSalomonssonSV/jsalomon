#' fetch_countries
#' @param key API key
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @export
#' @return df data frame

fetch_countries<-function(key=key){
  getdata<-httr::GET(url=paste0("https://apiservice.borsdata.se/v1/countries?authKey=", key))
  data_json <- httr::content(getdata, type="text", encoding = "UTF-8")
  df <- jsonlite::fromJSON(data_json,)
  df<-df$countries
  return(df)
}