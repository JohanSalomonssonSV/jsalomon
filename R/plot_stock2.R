#' plot_stock2
#' @param ticker ticker
#' @param plot_h lookback period in days
#' @importFrom tidyquant tq_get geom_candlestick
#' @importFrom ggplot2 ggplot geom_abline geom_col labs theme_minimal geom_line scale_fill_identity scale_color_identity aes
#' @importFrom geomtextpath geom_textline
#' @importFrom lubridate today
#' @importFrom dplyr mutate select filter lag
#' @importFrom patchwork wrap_plots
#' @importFrom utils combn
#' @importFrom stats lm
#' @importFrom purrr map_dfr map2
#' @importFrom roll roll_mean
#' @importFrom grDevices chull
#' @importFrom bdscale scale_x_bd
#' @importFrom scales date_format
#' @export
#' @return p
#' 
#' 



plot_stock2<-function(ticker, plot_h=350){
  # ticker<-"APRN"
  ticker <- ticker
  start <- lubridate::today()-365*2
  df1 <- tidyquant::tq_get(ticker, from = start) %>%
    dplyr::mutate(open = round(open,digits=2),
                  high = round(high,digits=2),
                  low = round(low,digits=2),
                  close = round(close,digits=2),
                  sma10= roll::roll_mean(close, width = 10),
                  sma20= roll::roll_mean(close, width = 20),
                  sma50= roll::roll_mean(close, width = 50),
                  sma100= roll::roll_mean(close, width = 100),
                  sma200= roll::roll_mean(close, width = 200),
                  sma_vol_10=roll::roll_mean(volume, width = 10),
                  sma_vol_20=roll::roll_mean(volume, width = 20),
                  sma_vol_50=roll::roll_mean(volume, width = 50),
                  adr=jsalomon::ADR_function(high, low)
    ) %>%
    dplyr::select(symbol,date,open,high,low,close,volume,adr, dplyr::starts_with("sma"))
  
  resistance_line<-function(data){
    data<- data |> mutate(rn=row_number())
    last_close<- data |> filter(date==max(date)) |> pull(close)
    highs <- data[chull(data[c("rn", "high")]),] %>%
      filter(date<max(date))
    
    all_highcombos <- bind_cols(as.data.frame(t(combn(highs$rn,m=2,simplify=TRUE))),as.data.frame(t(combn(highs$high,m=2,simplify=TRUE))))
    colnames(all_highcombos) <- c("X1","X2","Y1","Y2")
    
    n <- seq(1:nrow(all_highcombos))
    high_trendatainder <- function(n,all_highcombos){
      model <- lm(c(all_highcombos$Y1[n],all_highcombos$Y2[n])~c(all_highcombos$X1[n],all_highcombos$X2[n]))
      data.frame(intercept = model$coefficients[1],slope = model$coefficients[2])
    }
    
    high_trendlines <- map_dfr(n,high_trendatainder,all_highcombos = all_highcombos)
    
    high_trendline_test <- function(x,y,data){
      !any(x*as.numeric(max(data$rn))+y < 0.95*data$close[nrow(data)]) & y>=0
    }
    
    none_below <- map2(.x = high_trendlines$slope,.y = high_trendlines$intercept,.f = high_trendline_test,data = data)
    none_below <- unlist(none_below)
    high_trendlines <- high_trendlines[none_below,]
    high_trendlines<-tibble(high_trendlines)  |>
      mutate(pred=intercept+ slope*as.numeric(max(data$rn)+1),
             last_close=last_close,
             break_out=ifelse(last_close>pred, "Breakout", "Not")
      ) |>
      arrange(slope) 
    high_trendlines
  }
  
  
  
  
  
  high_trendlines<-resistance_line(df1)
  high_trendlines<-dplyr::filter(high_trendlines,slope<=0,
                                 last_close*1.1>=pred
  )
  df1<-df1 |> dplyr::mutate(rn=row_number())
  
  
  pred_mat<-function(x,data, trend_line_data){
    z<-bind_cols(date=paste0(lubridate::ymd(data$date)),
                 close=trend_line_data$intercept[x]+data$rn*trend_line_data$slope[x],
                 pred=paste0("pred_",x) )
    
    z
  }
  
  if (nrow(high_trendlines)>=1){ 
    t<-lapply(1:nrow(high_trendlines),function(x) pred_mat(x,df1, high_trendlines))
    t<-data.table::rbindlist(t) |> tibble()}
  if (nrow(high_trendlines)<1) {
    t<-tibble(date=lubridate::ymd(max(df1$date)), close=as.numeric(NA), pred=as.numeric(NA))
  }
  
  dd<-dplyr::filter(df1, date>=lubridate::today()-plot_h) 
  adr<-dd |> dplyr::filter(date==max(date)) |> dplyr::pull(adr)
  p<-  dd |>  ggplot2::ggplot(aes(x = date, y = close)) +
    tidyquant::geom_candlestick(aes(open = open, high = high, low = low, close = close),
                                colour_up   = "cyan"  ,
                                colour_down = "purple" ,
                                fill_up   = "cyan"  ,
                                fill_down = "purple" ) +
    ggplot2::geom_hline(aes(yintercept=ifelse(date==max(date), close,NA)), color="cyan",lty=3)+
    ggplot2::geom_line(data=t,
                       aes(lubridate::ymd(date),close, group=pred),
                       color="white",size=0.7)+
    geomtextpath::geom_textline(aes(y=sma10, label="10"),
                                size = 3, color = "pink",hjust = 0.2)+
    geomtextpath::geom_textline(aes(y=sma20, label="20"),
                                size = 3, color = "grey70",hjust = 0.2)+
    geomtextpath::geom_textline(aes(y=sma50, label="50"),
                                size = 3, color = "green",hjust = 0.2)+
    geomtextpath::geom_textline(aes(y=sma100, label="100"),
                                size = 3, color = "blue",hjust = 0.2)+
    geomtextpath::geom_textline(aes(y=sma200, label="200"),
                                size = 3, color = "red",hjust = 0.2)+
    scale_y_continuous(limits = c(min(dd$close)*0.9,max(dd$close)*1.1))+
    bdscale::scale_x_bd(business.dates=dd$date, max.major.breaks=10, labels=scales::date_format("%b\n'%y"))+
    labs(title = paste0(ticker,", adr: ",round(adr,1)), y = "Price", x = "") +
    
    jsalomon::theme_bors()
  
  
  v<-dd |>  ggplot2::ggplot(aes(x = date, y = volume))+
    ggplot2::geom_col(aes(fill=ifelse(close>dplyr::lag(close), "green", "red")))+
    ggplot2::geom_line(aes(y=sma_vol_50,group=1, color=ifelse(sma_vol_50>dplyr::lag(sma_vol_50), "cyan", "orange")))+
    ggplot2::scale_fill_identity()+
    ggplot2::scale_color_identity()+
    bdscale::scale_x_bd(business.dates=dd$date, max.major.breaks=10, labels=scales::date_format("%b\n'%y"))+
    labs(x="", caption = paste("Data: Yahoo! Finance. Accessed ",Sys.Date(),".",sep=""))+
    jsalomon::theme_bors()+
    theme(legend.position = "none",
          axis.text.y = element_blank()
    )
  
  layout <- "
AAAA
AAAA
AAAA
BBBB
"
  p<-patchwork::wrap_plots(p,v, design = layout) 
  p
  
}