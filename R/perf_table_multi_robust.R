#' perf_table_multi_robust
#' @param country nordic america
#' @importFrom purrr map_dfr map2_dfr
#' @export
#' @return screen_data screen_data
#' 
perf_table_multi_robust<-function(country="america"){
  if (country=="america"){
    screen_data<-purrr::map_dfr(c("NYSE","NASDAQ"), function(x){
      for (i in 1:10) {
      Sys.sleep(i)
        
      screen_data <-try({jsalomon::perf_table(country = country,exchange=x )})
      if (!inherits(screen_data, "try-error")) {
        print("Successfully getting the data")
        screen_data
        break
      }}
      screen_data})
    }
  if (country=="nordic"){
    screen_data <- purrr::map2_dfr(c("sweden",
                                     "sweden",
                                     "denmark",
                                     "norway",
                                     "finland"),
                                   c("OMXSTO",
                                     "NGM",
                                     "OMXCOP",
                                     "OSL",
                                     "OMXHEX"),
                                   function(country, exchange) {
                                     for (i in 1:10) {
                                       Sys.sleep(i)
                                       
                                       screen_data <-
                                         try({
                                           jsalomon::perf_table(country = country, exchange = exchange)
                                         })
                                       if (!inherits(screen_data, "try-error")) {
                                         print("Successfully getting the data")
                                         screen_data
                                         break
                                       }
                                     }
                                     screen_data
                                   })
 
  }
}
#a<-perf_table_multi_robust("nordic")  
