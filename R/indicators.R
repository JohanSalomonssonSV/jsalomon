#' indicators
#' @param data data 
#' @param high high
#' @param low low
#' @param close close
#' @param open open
#' @importFrom TTR BBands MACD RSI WPR stoch SMA
#' @importFrom purrr map_dfc set_names
#' @importFrom dplyr bind_cols
#' @export
#' @return df
#' 
indicators<-function(data,high=high, low=low, close=close, open=open ){
  # BB bands
  bb<-TTR::BBands(data[,c("high","low","close")] )
  colnames(bb)<- c("lower_bb", "sma_bb", "upper_bb", "pct_bb")
  # MACD
  macd<- TTR::MACD(data[,"close"])
  colnames(macd)<-c("macd", "signal_macd")
  # RSI
  rsi<-data.frame(rsi=TTR::RSI(data[,"close"], n=14))
  #WPR
  wpr<-data.frame(wpr=TTR::WPR(df[,c("high","low", "close")], n = 14))
  # Stochastic
  sto<-TTR::stoch(data[,c("high","low","close")])
  colnames(sto)<-c("stoch_fastK", "stoch_fastD", "stoch_slowD")
  # SMA
  sma<-purrr::map_dfc(c(7,10,20,50,100,200),
                     function(x){
                       TTR::SMA(data[,c("close")], n=x) 
                     }
  ) |> purrr::set_names("sma7","sma10","sma20","sma50","sma100","sma200")
  z<-dplyr::bind_cols(bb,rsi,macd,sto,wpr,sma)
  z
  
}