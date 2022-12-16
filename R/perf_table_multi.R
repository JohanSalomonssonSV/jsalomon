#' perf_table_multi
#' @param country nordic america
#' @importFrom purrr map_dfr map2_dfr
#' @export
#' @return screen_data screen_data
#' 
perf_table_multi<-function(country="america"){
  if (country=="america"){
    screen_data<-purrr::map_dfr(c("NYSE","NASDAQ"), function(x){
      screen_data <-jsalomon::perf_table(country = country,exchange=x )
      screen_data
      }
            )
    
    
  }
  if (country=="nordic"){
    screen_data<-purrr::map2_dfr(c("sweden", 
                            "denmark",
                            "norway", 
                            "finland"),
                          c("OMXSTO",
                            "OMXCOP",
                            "OSL",
                            "OMXHEX"), 
                          function(country, exchange){
                              screen_data<-jsalomon::perf_table(country = country,exchange=exchange )
                              screen_data
                              }
    )
  }
  screen_data
 }
#a<-perf_table_multi("nordic")  
