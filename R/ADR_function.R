#' ADR_function
#' @param high high
#' @param low low
#' @param days days
#' @importFrom roll roll_sum
#' @export
#' @return ADR
ADR_function<-\( high="high", low="low", days=20){
  ADR<-(roll::roll_sum(high/low-1, width = days)/days)*100
  return(ADR)
}