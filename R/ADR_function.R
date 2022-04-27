#' ADR_function
#' @param high high
#' @param low low
#' @importFrom roll roll_sum
#' @export
#' @return ADR
ADR_function<-\( high="high", low="low"){
  ADR<-(roll::roll_sum(high/low-1, width = 20)/20)*100
  return(ADR)
}