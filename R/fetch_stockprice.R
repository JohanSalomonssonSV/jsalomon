#' fetch_stockprice
#' @param key API key
#' @param id Instrument id
#' @param since A date
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr filter mutate
#' @importFrom magrittr "%>%"
#' @export
#' @return kurs data frame

fetch_stockprice<-function(id,since="2018-01-01", key=Sys.getenv("BORSDATA_KEY")){
  endpoint <- paste0("/v1/instruments/", id,"/stockprices")
  getdata<-httr::GET(url=paste0("https://apiservice.borsdata.se", endpoint, "?authKey=", key, "&maxcount=20"))
  data_json <- httr::content(getdata, type="text", encoding = "UTF-8")
  df <- jsonlite::fromJSON(data_json,)
  kurs<-df$stockPricesList %>% dplyr::filter(d>=since)%>%  dplyr::mutate(insId=id)
  
  return(kurs)
}