#' fetch_branches
#' @param key API key
#' @export
#' @return x Table connection

fetch_branches<-function(key=key){
  getdata<-httr::GET(url=paste0("https://apiservice.borsdata.se/v1/branches?authKey=", key))
  data_json <- httr::content(getdata, type="text", encoding = "UTF-8")
  df <- jsonlite::fromJSON(data_json,)
  df<-df$branches
  return(df)
}