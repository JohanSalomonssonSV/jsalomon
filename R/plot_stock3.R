#' plot_stock3 test
#' @param ticker ticker
#' @param plot_h lookback period in days
#' @param zoom_days zoom days
#' @importFrom tidyquant tq_get geom_candlestick
#' @importFrom ggplot2 ggplot geom_abline geom_tile geom_col geom_label labs theme_minimal geom_line scale_fill_identity scale_color_identity geom_segment scale_fill_manual
#' @importFrom geomtextpath geom_textline
#' @importFrom lubridate today floor_date
#' @importFrom dplyr mutate select filter lag lead pull summarize
#' @importFrom patchwork wrap_plots
#' @importFrom utils combn
#' @importFrom stats lm
#' @importFrom purrr map_dfr map2
#' @importFrom roll roll_mean roll_quantile roll_max
#' @importFrom grDevices chull
#' @importFrom bdscale scale_x_bd
#' @importFrom scales date_format
#' @importFrom TTR BBands
#' @export
#' @return p
#' 
#' 



plot_stock3<-function(ticker, plot_h=350, zoom_days=55){
  # ticker<-"SOBR"
  ticker <- ticker
  start <- lubridate::today()-365*2
  df1 <- tidyquant::tq_get(ticker, from = start) %>%
    filter(!is.na(close)) |> 
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
                  sma_vol_dollar_20=roll::roll_mean(volume*close, width=20),
                  
                  adr=jsalomon::ADR_function(high, low)
    ) %>%
    dplyr::select(symbol,date,open,high,low,close,volume,adr, dplyr::starts_with("sma"))
  
  bb<-TTR::BBands(df1[,c("high","low","close")] )
  colnames(bb)<- c("lower_bb", "sma_bb", "upper_bb", "pct_bb")
 
df1<-  bind_cols(df1, bb) |> 
    mutate(tight=ifelse(((upper_bb/lower_bb)-1)<=roll::roll_quantile( ((upper_bb/lower_bb)-1),width = 60,  p=0.01 )&
                          close>=sma50 &
                          sma50>lag(sma50,2)
                        #close>=sma200
                        , high, NA),
           tight_gain=ifelse(!is.na(tight), round(((lead(roll::roll_max(close, width = 30),30)/ close)-1)*100,1),NA )
    )
  
  
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
    
    high_trendlines<-high_trendlines |> bind_cols(all_highcombos |> 
                                                    mutate(first=ifelse(X1<X2, X1,X2)) |> 
                                                    select(first)
    )
    
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
  high_trendlines<-dplyr::filter(high_trendlines,slope<=0 | (pred<=(1.05*last_close) & first>=floor(0.5*nrow(df1)) ) #,
                                 #last_close*1.2>=pred
  )
  df1<-df1 |> dplyr::mutate(rn=row_number())
  
  
  pred_mat<-function(x,data, trend_line_data){
    type_of_predmat<-any(str_detect(names(trend_line_data),"first"))
    #data |> arrange(desc(date)) |> head(5)
    
    if (type_of_predmat==TRUE){
      z<-bind_cols(date=paste0(lubridate::ymd(data$date)),
                   close=data$close,
                   first=data$date[data$rn==trend_line_data$first[x]],
                   pred_value=trend_line_data$intercept[x]+data$rn*trend_line_data$slope[x],
                   pred=paste0("pred_",x) ) |> 
        filter(!date<first) |> 
        mutate(close_above_pred=ifelse(any(close[date!=max(date)]>pred_value[date!=max(date)]), 1,0))
      
      z} 
    else {
      z<-bind_cols(date=paste0(lubridate::ymd(data$date)),
                   close=data$close,
                   pred_value=trend_line_data$intercept[x]+data$rn*trend_line_data$slope[x],
                   pred=paste0("pred_",x) ) 
      
      z
    }
  }
  
  if (nrow(high_trendlines)>=1){ 
    t<-lapply(1:nrow(high_trendlines),function(x) pred_mat(x,df1, high_trendlines))
    t<-data.table::rbindlist(t) |> tibble()
    t<-t |> filter(close_above_pred==0) |> select(-close_above_pred)
  }
  if (nrow(high_trendlines)<1) {
    t<-tibble(date=lubridate::ymd(max(df1$date)),close=as.numeric(NA), pred_value=as.numeric(NA), pred=as.numeric(NA))
  }
  
  
  ## HERE ##
  
  support_line<-function(data){
    data<- data |> mutate(rn=row_number())
    last_close<- data |> filter(date==max(date)) |> pull(close)
    lows <- data[grDevices::chull(data[c("rn", "low")]),] %>%
      dplyr::filter(date<max(date))%>%
      filter(date<max(date))
    
    
    all_lowcombos <- dplyr::bind_cols(as.data.frame(t(utils::combn(lows$rn,m=2,simplify=TRUE))),as.data.frame(t(combn(lows$low,m=2,simplify=TRUE))))
    colnames(all_lowcombos) <- c("X1","X2","Y1","Y2")
    
    n <- seq(1:nrow(all_lowcombos))
    low_trendfinder <- function(n,all_lowcombos){
      model <- lm(c(all_lowcombos$Y1[n],all_lowcombos$Y2[n])~c(all_lowcombos$X1[n],all_lowcombos$X2[n]))
      data.frame(intercept = model$coefficients[1],slope = model$coefficients[2])
    }
    low_trendlines <- purrr::map_dfr(n,low_trendfinder,all_lowcombos = all_lowcombos)
    
    low_trendline_test <- function(x,y,data){
      !any(x*as.numeric(data$rn) + y > data$low + 0.01) #& !(x*as.numeric(Sys.Date())+y < 0.5*data$close[nrow(data)])
    }
    none_below <- map2(.x = low_trendlines$slope,.y = low_trendlines$intercept,.f = low_trendline_test,data = data)
    none_below <- unlist(none_below)
    low_trendlines <- low_trendlines[none_below,]
    
    low_trendlines<-tibble(low_trendlines)  |>
      mutate(pred=intercept+ slope*as.numeric(max(data$rn)+1),
             last_close=last_close,
             break_out=ifelse(last_close<pred, "Breakdown", "Not")
      ) |>
      arrange(-slope) 
    
    low_trendlines
  }
  
  low_trendlines<-support_line(df1)
  
  low_trendlines<-dplyr::filter(low_trendlines,slope>=0)
  
  if (nrow(low_trendlines)>=1){ 
    l<-lapply(1:nrow(low_trendlines),function(x) pred_mat(x,df1, low_trendlines))
    l<-data.table::rbindlist(l) |> tibble()}
  if (nrow(low_trendlines)<1) {
    l<-tibble(date=lubridate::ymd(max(df1$date)), pred_value=as.numeric(NA), pred=as.numeric(NA))
  }
  
  ######  END Supportline
  
  dd<-dplyr::filter(df1, date>=lubridate::today()-plot_h) 
  #adr<-dd |> dplyr::filter(date==max(date)) |> dplyr::pull(adr)
  max_date<-max(dd$date)
  limit_date<-dd$date[nrow(dd)-zoom_days]
  max_high<- max(dd$high)
  min_low<- min(dd$low)
  
  BO_df<- filter(t,date==max(date) ) |> 
    mutate(break_out=ifelse(close>pred_value, "BO",NA ))
  
  disp<-dd |> dplyr::filter(date==max(date)) |> 
    mutate(tick=factor(1))|> 
    mutate(adr=as.character(round(adr,1)), 
           close=as.character(round(close,2)),
           sma_vol_20=paste0(round(sma_vol_20/1000000,2),"M"),
           sma_vol_dollar_20=paste0(round(sma_vol_dollar_20/1000000,2),"M")
           
    ) |> 
    select(tick,date,"Adr"=adr, "Close"=close, 
           "Vol. avg."=sma_vol_20, "Vol.Dollar avg."=sma_vol_dollar_20) |> 
    pivot_longer(3:6) |> 
    mutate(name=ifelse(name=="Close", paste0(name,"(",date,")"), name))
  
  dp<-disp |> 
    ggplot2::ggplot(aes(y=tick,x=name, label=paste0(name, ": ", value), fill=name ))+
    ggplot2::geom_tile()+
    ggplot2::geom_text(family ="mono", fontface="bold")+
    ggplot2::scale_fill_manual(values = c("#6dd3ce", "#c8e9a0", "#f7a278", "#a13d63"))+
    jsalomon::theme_bors()+
    labs(title = NULL, x=NULL, y=NULL,caption = paste0("@salojoh | Data: Yahoo! Finance. Accessed ",Sys.Date(),"."))+
    theme(legend.position = "none",
          #text = element_text(family="serif"),
          axis.text = element_blank(),
          axis.title = element_blank())
  
  
  
  p<-  dd |>  ggplot2::ggplot(aes(x = date, y = close)) +
    ggplot2::geom_text(aes(label=ifelse(date==lubridate::floor_date(median(date)), paste0(symbol), NA), y=((max(close)-min(close))/2)+min(close)  ),
                       color="white",
                       size=26,
                       alpha=0.1
    )+
    tidyquant::geom_candlestick(aes(open = open, high = high, low = low, close = close),
                                colour_up   = "cyan"  ,
                                colour_down = "purple" ,
                                fill_up   = "cyan"  ,
                                fill_down = "purple" ) +
    ggplot2::geom_hline(aes(yintercept=ifelse(date==max(date), close,NA)), color="cyan",lty=5,size=0.2)+
    ggplot2::geom_line(data=t,
                       aes(lubridate::ymd(date),pred_value, group=pred),
                       color="grey70",size=0.2)+
    ggplot2::geom_line(data=l,
                       aes(lubridate::ymd(date),pred_value, group=pred),
                       color="yellow",
                       size=0.2)+
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
    ggplot2::geom_label(data=filter(dd,date==max(date)),
                        aes(label=round(close,2),
                            #y=close, 
                            x=min(dd$date) 
                        ),
                        #hjust=-.1,
                        fill="black",
                        size=2.5,
                        color="cyan"
    )+
    ggplot2::geom_text(label=paste0("H: ",max_high), y=max_high, x=-Inf,
                       hjust=-1,
                       color="#c8e9a0",
                       size=3
    )+
    ggplot2::geom_text(label=paste0("L: ",min_low), y=min_low, x=-Inf,
                       hjust=-1,
                       color="#f7a278",
                       size=3
    )+
    ggplot2::geom_text(
      data = BO_df,
      aes(label = ifelse(break_out == "BO", "B", NA)),
      x = Inf,
      y = max_high,
      hjust = 2,
      color = "yellow"
    )+
    ggplot2::geom_point(aes(y=tight), color="green", alpha=0.5)+
    ggplot2::geom_segment(aes(
      x= date,xend=date,
      y=ifelse(!is.na(tight_gain),high,NA), 
      yend=high*((tight_gain/100)+1))
      ,color="green"
      , size=0.3 )+
    ggplot2::geom_text(aes(label=paste0(tight_gain,"%"), x=date, y=high*((tight_gain/100)+1)  )
              ,check_overlap = T, vjust="left", color="green", size=2.5)+
    ggplot2::scale_y_continuous(limits = c(min(dd$low)*0.9,max(dd$high)*1.1),
                       sec.axis = sec_axis( trans=~.))+
    bdscale::scale_x_bd(business.dates=dd$date, max.major.breaks=10, labels=scales::date_format("%b\n'%y"))+
    labs(title = paste(ticker),y=NULL, x = "") +
    #scale_y_continuous(sec.axis = sec_axis( trans=~.))+
    jsalomon::theme_bors()+
    theme(axis.text.x=element_blank(),
          axis.title.x =element_blank())
  
  
  
  pz<-p+ggplot2::coord_cartesian(xlim = c(limit_date,max_date
  ),
  ylim = c( dplyr::filter(dd,date>=limit_date) |> dplyr::summarize(min_low=min(low)*0.95 ) |> dplyr::pull(min_low),
            dplyr::filter(dd,date>=limit_date) |> dplyr::summarize(max_high=max(high)*1.05 ) |> dplyr::pull(max_high) )
  
  )+
    labs(title =NULL,subtitle =  paste0("Recent days"), x=NULL, y=NULL)+
    theme(#axis.text.x=element_blank(),
      axis.title.x =element_blank())
  
  
  v<-dd |>  ggplot2::ggplot(aes(x = date, y = volume))+
    ggplot2::geom_col(aes(fill=ifelse(close>dplyr::lag(close), "green", "red")))+
    ggplot2::geom_line(aes(y=sma_vol_50,group=1, color=ifelse(sma_vol_50>dplyr::lag(sma_vol_50), "blue", "orange")))+
    ggplot2::scale_fill_identity()+
    ggplot2::scale_color_identity()+
    bdscale::scale_x_bd(business.dates=dd$date, max.major.breaks=10, labels=scales::date_format("%b\n'%y"))+
    labs(x="",y=NULL#, caption = paste("Data: Yahoo! Finance. Accessed ",Sys.Date(),".",sep="")
    )+
    jsalomon::theme_bors()+
    theme(legend.position = "none",
          axis.text.y = element_blank(),
          axis.title.x =element_blank()
    )
  vz<- v+coord_cartesian(xlim = c(limit_date,max_date),
                         ylim = c( dplyr::filter(dd,date>=limit_date) |> dplyr::summarize(min_vol=min(volume)*0.95 ) |> dplyr::pull(min_vol),
                                   dplyr::filter(dd,date>=limit_date) |> dplyr::summarize(max_vol=max(volume)*1.05 ) |> dplyr::pull(max_vol) )
  )+
    labs(title = NULL, x=NULL, y=NULL#,caption = paste("@salojoh | Data: Yahoo! Finance. Accessed ",Sys.Date(),".",sep="")
    )+
    theme(axis.text.x=element_blank(),
          axis.title.x =element_blank() )
  
  
  
  layout <- "
AAAAAADD
AAAAAADD  
AAAAAADD
AAAAAADD
AAAAAADD
AAAAAADD
AAAAAADD
BBBBBBEE
BBBBBBEE
CCCCCCCC
"
  p<-patchwork::wrap_plots(p,v,dp,pz,vz, design = layout) 
  p
  #patchwork::plot_annotation(p,theme(text = element_text('mono')))
}

#plot_stock3("KRYS", 350)
