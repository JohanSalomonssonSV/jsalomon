#' fetch_multiple_stock
#' @param df a subset of companies 
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr pull mutate left_join
#' @importFrom tibble tibble
#' @importFrom magrittr "%>%"
#' @importFrom lubridate ymd
#' @importFrom data.table rbindlist
#' @export
#' @return df data frame

fetch_multiple_stock<-function(df){
  b<-df %>% pull(insId)
  
  z<-lapply(b ,fetch_stockprice) %>% data.table::rbindlist(.) %>% 
    tibble() %>% 
    left_join(df) %>% 
    mutate(d=lubridate::ymd(d))
  z
  
}