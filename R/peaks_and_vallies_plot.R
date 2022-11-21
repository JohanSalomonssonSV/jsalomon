#' peaks_and_vallies_plot
#' @param ticker ticker
#' @param start_date start_date
#' @param M size
#' @importFrom tidyquant geom_candlestick
#' @importFrom ggplot2 ggplot geom_abline geom_tile geom_col geom_label labs theme_minimal geom_line scale_fill_identity geom_vline scale_color_identity geom_segment scale_alpha_identity scale_fill_manual
#' @importFrom dplyr mutate select filter lag summarize
#' @importFrom scales date_format
#' @importFrom bdscale scale_x_bd
#' @export
#' @return p

peaks_and_vallies_plot<-function(ticker="^GSPC", start_date="2021-01-01",M=12){
  #ticker="AAPL"
  data<-jsalomon::getstk(ticker)
  
  data <- dplyr::filter(data, date>=start_date)
  
  peaks<-jsalomon::find_peaks(data$high, M)
  valley<- jsalomon::find_peaks(-data$low)
  
  data<-data |> dplyr::mutate(ro=dplyr::row_number(),
                       peaks=ifelse(ro %in% peaks, high,NA),
                       valley=ifelse(ro %in% valley, low,NA),
  ) 
  peaks<-dplyr::filter(data, !is.na(peaks)) |> dplyr::mutate(day_peaks=ro-dplyr::lag(ro))
  valley<-dplyr::filter(data, !is.na(valley)) |> dplyr::mutate(day_valley=ro-dplyr::lag(ro))
  
  p<-data |> 
    ggplot2::ggplot(aes(date))+
    tidyquant::geom_candlestick(aes(open = open, high = high, low = low, close = close),
                                colour_up   = "cyan"  ,
                                colour_down = "purple" ,
                                fill_up   = "cyan"  ,
                                fill_down = "purple" )+
    ggplot2::geom_point(data=peaks,aes(date, peaks), shape=22, color="red", size=3)+
    ggplot2::geom_label(data=peaks, aes(label=paste0(round(peaks,0),"\nLast peak: ",day_peaks,"d"), date, peaks*1.03), 
               color="red",
               fill="grey2",
               vjust="left", 
               size=2)+
    ggplot2::geom_point(data=valley,aes(date, valley), shape=22,color="green", size=3)+
    ggplot2::geom_label(data=valley, aes(label=paste0(round(valley,0),"\nLast valley: ",day_valley,"d"), date, valley*0.97), 
               color="green",
               fill="grey2",
               vjust="right", 
               size=2)+
    bdscale::scale_x_bd(business.dates=data$date, max.major.breaks=10, labels=scales::date_format("%b\n'%y"))+
    jsalomon::theme_bors()+
    ggplot2::labs(title=paste0(ticker), x=NULL, y=NULL)
  p
  
}

