#' fetch_volume_50d_5d_for_all
#' @param key API key 
#' @importFrom dplyr select left_join
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @export
#' @return kurs data frame
fetch_volume_50d_5d_for_all<-function(key=Sys.getenv("BORSDATA_KEY")){
  
  url <- paste0("https://apiservice.borsdata.se/v1/instruments/kpis/313/50day/mean?authKey=",key)
  getdata<-httr::GET(url)
  data_json <- httr::content(getdata, type="text", encoding = "UTF-8")
  df_all <- jsonlite::fromJSON(data_json,)
  x<-df_all$values |>    dplyr::select("insId"=i, "avg_50d_volume"=n)
  
  url1 <- paste0("https://apiservice.borsdata.se/v1/instruments/kpis/313/5day/mean?authKey=",key)
  getdata1<-httr::GET(url1)
  data_json1 <- httr::content(getdata1, type="text", encoding = "UTF-8")
  df_all1 <- jsonlite::fromJSON(data_json1,)
  y<-df_all1$values |>    dplyr::select("insId"=i, "avg_5d_volume"=n)
  
  kurs<-x |> dplyr::left_join(y)
  
  return(kurs)
}