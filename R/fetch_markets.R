#' fetch_markets
#' @param key API key
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @export
#' @return df data frame
fetch_markets<-function(key=key){
  getdata<-httr::GET(url=paste0("https://apiservice.borsdata.se/v1/markets?authKey=", key))
  data_json <- httr::content(getdata, type="text", encoding = "UTF-8")
  df <- jsonlite::fromJSON(data_json,)
  df<-df$markets  # Sektorerna
  return(df)
}