#' plot_stock
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
#' @export
#' @return p
#' 
#' 
plot_stock<-function(ticker, plot_h=300){

ticker <- ticker
start <- lubridate::today()-365*2
df1 <- tq_get(ticker, from = start) %>%
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
         sma_vol_50=roll::roll_mean(volume, width = 50)#,
         #adr=jsalomon::ADR_function(high,low)
  ) %>%
  dplyr::select(symbol,date,open,high,low,close,volume, dplyr::starts_with("sma"))

support_line<-function(df){
  lows <- df[grDevices::chull(df[c("date", "low")]),] %>%
    dplyr::filter(date<max(date))
  # Find all unique possible combinations of two lows
  # (and all unique possible combinations of their associated dates)
  all_lowcombos <- dplyr::bind_cols(as.data.frame(t(utils::combn(lows$date,m=2,simplify=TRUE))),as.data.frame(t(combn(lows$low,m=2,simplify=TRUE))))
  colnames(all_lowcombos) <- c("X1","X2","Y1","Y2")
  # Generate a trend line for every combination of points
  n <- seq(1:nrow(all_lowcombos))
  low_trendfinder <- function(n,all_lowcombos){
    model <- lm(c(all_lowcombos$Y1[n],all_lowcombos$Y2[n])~c(all_lowcombos$X1[n],all_lowcombos$X2[n]))
    data.frame(intercept = model$coefficients[1],slope = model$coefficients[2])
  }
  low_trendlines <- purrr::map_dfr(n,low_trendfinder,all_lowcombos = all_lowcombos)
  # For each low_trendline, check if any low in the prices dataframe falls below the line
  # Keep only trendlines for which this is FALSE
  # Also make sure the trendline wouldn't be less than half the current price for today's date
  low_trendline_test <- function(x,y,df){
    !any(x*as.numeric(df$date) + y > df$low + 0.01) & !(x*as.numeric(Sys.Date())+y < 0.5*df$close[nrow(df)])
  }
  none_below <- map2(.x = low_trendlines$slope,.y = low_trendlines$intercept,.f = low_trendline_test,df = df)
  none_below <- unlist(none_below)
  low_trendlines <- low_trendlines[none_below,]
  low_trendlines
}

low_trendlines<-support_line(df1)


resistance_line<-function(df){
  
  highs <- df[grDevices::chull(df[c("date", "high")]),] %>%
    filter(date<max(date))
  
  # Find all unique possible combinations of two lows
  # (and all unique possible combinations of their associated dates)
  all_highcombos <- dplyr::bind_cols(as.data.frame(t(combn(highs$date,m=2,simplify=TRUE))),as.data.frame(t(combn(highs$high,m=2,simplify=TRUE))))
  colnames(all_highcombos) <- c("X1","X2","Y1","Y2")
  
  # Generate a trend line for every combination of points
  n <- seq(1:nrow(all_highcombos))
  high_trendfinder <- function(n,all_highcombos){
    model <- lm(c(all_highcombos$Y1[n],all_highcombos$Y2[n])~c(all_highcombos$X1[n],all_highcombos$X2[n]))
    data.frame(intercept = model$coefficients[1],slope = model$coefficients[2])
  }
  high_trendlines <- purrr::map_dfr(n,high_trendfinder,all_highcombos = all_highcombos)
  
  # For each low_trendline, check if any low in the prices dataframe falls below the line
  # Keep only trendlines for which this is FALSE
  # Also make sure the trendline wouldn't be less than half the current price for today's date
  high_trendline_test <- function(x,y,df){
    # !any(x*as.numeric(df$date) + y > df$high*0.5 #+ 0.01
    #      ) &
    !(x*as.numeric(lubridate::today())+y < 0.95*df$close[nrow(df)]) & y>=0
  }
  none_below <- purrr::map2(.x = high_trendlines$slope,.y = high_trendlines$intercept,.f = high_trendline_test,df = df)
  none_below <- unlist(none_below)
  high_trendlines <- high_trendlines[none_below,]
  high_trendlines
}


high_trendlines<-resistance_line(df1)
# 
#[high_trendlines$slope<=0] 
# 
p<-dplyr::filter(df1, date>=lubridate::today()-plot_h) %>%
  ggplot2::ggplot(aes(x = date, y = close)) +
  tidyquant::geom_candlestick(aes(open = open, high = high, low = low, close = close),
                   colour_up   = "cyan"  ,
                   colour_down = "purple" ,
                   fill_up   = "cyan"  ,
                   fill_down = "purple" ) +
  geom_abline(intercept=low_trendlines$intercept,slope=low_trendlines$slope, color=scales::alpha("yellow", 0.5)) +
  geom_abline(intercept=high_trendlines$intercept,slope=high_trendlines$slope, color=scales::alpha("white", 0.5), lty=3 ) +
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
  # geom_line(aes(y=sma10), color="pink")+
  # geom_line(aes(y=sma20), color="grey70")+
  # geom_line(aes(y=sma50), color="green")+
  # geom_line(aes(y=sma100), color="blue")+
  # geom_line(aes(y=sma200), color="red")+
  labs(title = paste(ticker,"Trendline Chart"), y = "Price", x = "") +
  #theme_tq()
  jsalomon::theme_bors()

v<-dplyr::filter(df1, date>=lubridate::today()-plot_h) %>%
  ggplot2::ggplot(aes(x = date, y = volume))+
  ggplot2::geom_col(aes(fill=ifelse(close>dplyr::lag(close), "green", "red")))+
  ggplot2::geom_line(aes(y=sma_vol_50,group=1, color=ifelse(sma_vol_50>dplyr::lag(sma_vol_50), "cyan", "orange")))+
  ggplot2::scale_fill_identity()+
  ggplot2::scale_color_identity()+
  labs(x="", caption = paste("Price data courtesy of Yahoo! Finance. Accessed ",Sys.Date(),".",sep=""))+
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

 
