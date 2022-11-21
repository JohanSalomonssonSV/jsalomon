#' convert_to_yf
#' @param TVdata TVdata
#' @importFrom dplyr case_when mutate across
#' @importFrom tibble tibble
#' @importFrom janitor clean_names
#' @export
#' @return TVdata

clean_TVdata<-function(TVdata){
  TVdata<-TVdata |> janitor::clean_names() |> tibble()
  
  TVdata<-TVdata |> 
    mutate(across(.cols= jsalomon::check_possible_numeric(TVdata), as.numeric))
  
  return(TVdata)
}