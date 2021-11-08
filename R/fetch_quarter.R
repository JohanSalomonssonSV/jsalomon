#' fetch_quarter
#' @param key API key
#' @param id ins id
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @export
#' @return df data frame

fetch_quarter<-function(id, key=Sys.getenv("BORSDATA_KEY")){
  endpoint <- paste0("/v1/instruments/", id,"/reports/quarter")
  getdata<-httr::GET(url=paste0("https://apiservice.borsdata.se", endpoint, "?authKey=", key, "&maxcount=40"))
  data_json <- httr::content(getdata, type="text", encoding = "UTF-8")
  df <- jsonlite::fromJSON(data_json,)
  df$reports$insId<-id
  return(df$reports)
}
