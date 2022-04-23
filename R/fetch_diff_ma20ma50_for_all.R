#' fetch_diff_ma20ma50_for_all
#' @param key API key
#' @importFrom dplyr select
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @export
#' @return kurs data frame
fetch_diff_ma20ma50_for_all<-function(key=Sys.getenv("BORSDATA_KEY")){
  #158	ma20ma50	diff	[MA / MA] diff ma20ma50
  # c(10,20,30,50,100)
  url <- paste0("https://apiservice.borsdata.se/v1/instruments/kpis/158/ma20ma50/diff?authKey=",key)
  getdata<-httr::GET(url)
  data_json <- httr::content(getdata, type="text", encoding = "UTF-8")
  df_all <- jsonlite::fromJSON(data_json,)
  kurs<-df_all$values |>    dplyr::select("insId"=i, ma20ma50_pct=n)
  
  return(kurs)
}