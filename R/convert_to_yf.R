#' convert_to_yf
#' @param symbol symbol
#' @param exchange exchange
#' @importFrom dplyr case_when 
#' @export
#' @return ticker
convert_to_yf<-function(symbol, exchange=exchange){
x<-case_when(
  exchange == "OMXSTO" ~ paste0(symbol, ".ST"),
  exchange == "OMXCOP" ~ paste0(symbol, ".CO"),
  exchange == "OSL" ~ paste0(symbol, ".OL"),
  exchange == "OMXHEX" ~ paste0(symbol, ".HE")
)
return(x)
}