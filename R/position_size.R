#' position_size
#' @param risk_in_money risk_in_money 
#' @param price_entry price_entry
#' @param stoploss stoploss
#' @export
#' @return df
#' 
position_size<-function(risk_in_money, price_entry, stoploss){
  position_quantity<-risk_in_money/(price_entry-stoploss)
  position_price_quantity<- position_quantity*price_entry
  return(cbind(position_quantity,position_price_quantity))
}
