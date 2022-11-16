#' check_possible_numeric
#' @param data data
#' @importFrom dplyr pull 
#' @importFrom purrr map_lgl
#' @export
#' @return vector index
check_possible_numeric <- function(data) {
  check_numeric_sample <- function(x) {
    temp<-data[,x] |> pull()
    temp<-temp[!is.na(temp)]
    temp<-sample(temp, 20)
    
    x <- any(!is.na(as.numeric(temp )))
    x
  }
  return(which(map_lgl(1:ncol(data), check_numeric_sample)))
}