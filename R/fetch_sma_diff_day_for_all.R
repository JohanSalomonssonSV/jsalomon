#' fetch_sma_diff_day_for_all
#' @param key API key 
#' @param days days
#' @importFrom dplyr select
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @export
#' @return kurs data frame
fetch_sma_diff_day_for_all<-function(days=50,key=Sys.getenv("BORSDATA_KEY")){
  
  # c(10,20,30,50,100)
  url <- paste0("https://apiservice.borsdata.se/v1/instruments/kpis/318/",days,"day/diff?authKey=",key)
  getdata<-httr::GET(url)
  data_json <- httr::content(getdata, type="text", encoding = "UTF-8")
  df_all <- jsonlite::fromJSON(data_json,)
  kurs<-df_all$values |>    dplyr::select("insId"=i, !!paste0("diff_sma",days, "_pct"):=n)
  
  return(kurs)
}