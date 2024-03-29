#' plot_stock5 test
#' @param ticker ticker
#' @param plot_h lookback period in days
#' @param zoom_days zoom days
#' @param title_p plot titles
#' @param pos_col col
#' @param neg_col col
#' @param tight_col col
#' @importFrom tidyquant tq_get geom_candlestick
#' @importFrom ggplot2 ggplot geom_abline geom_ribbon geom_tile geom_col geom_label labs theme_minimal geom_line scale_fill_identity geom_vline scale_color_identity geom_segment scale_alpha_identity scale_fill_manual
#' @importFrom geomtextpath geom_textline
#' @importFrom lubridate today floor_date
#' @importFrom dplyr mutate select filter lag lead pull summarize
#' @importFrom patchwork wrap_plots
#' @importFrom utils combn
#' @importFrom stats lm
#' @importFrom purrr map_dfr map2
#' @importFrom roll roll_mean roll_quantile roll_max roll_idxmax
#' @importFrom grDevices chull
#' @importFrom bdscale scale_x_bd
#' @importFrom scales date_format alpha
#' @importFrom TTR BBands EMA
#' @importFrom ggtext geom_richtext
#' @importFrom glue glue
#' @export
#' @return p
#' 
#' 



plot_stock5<-function(ticker, plot_h=400, zoom_days=55, title_p=FALSE, pos_col="#1ED01E",neg_col="#DC0101", tight_col="white" ){
  # ticker<-"AAPL"
  ticker <- ticker
  start <- lubridate::today()-plot_h*3
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
                  
                  ema10=TTR::EMA(close, n=10),
                  ema21=TTR::EMA(close, n=21),
                  
                  adr=jsalomon::ADR_function(high, low)
    ) %>%
    dplyr::select(symbol,date,open,high,low,close,volume,adr, dplyr::starts_with("sma"),dplyr::starts_with("ema") )
  
  bb<-TTR::BBands(df1[,c("high","low","close")] )
  colnames(bb)<- c("lower_bb", "sma_bb", "upper_bb", "pct_bb")
  
  df1<-  
    bind_cols(df1, bb) |> 
    mutate(tight=ifelse(((upper_bb/lower_bb)-1)<=roll::roll_quantile( ((upper_bb/lower_bb)-1),width = 60,  p=0.01 )&
                          close>=sma50 &
                          sma50>lag(sma50,2)
                        #close>=sma200
                        , high*1.05, NA),
           tight_gain=ifelse(!is.na(tight), round(((lead(roll::roll_max(close, width = 30),30)/ close)-1)*100,1),NA ),
           days_hold=lead(roll::roll_idxmax(close, width = 30),30)
    ) #|> select(date,date_span_end,tight, tight_gain, tight_gain_date,tight_gain_date_nr) |> filter(!is.na(tight))
  
  
  
  
  
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
  
minichart<-
  df1 |> ggplot2::ggplot(aes(date, close))+
  ggplot2::geom_rect(xmin=min(dd$date), xmax=max(dd$date),
              ymin=Inf,ymax=-Inf,alpha=0.5,
              color="black")+
  ggplot2::geom_line(aes(color=ifelse(sma10>=sma20 & 
                                 sma10>=lag(sma10) & 
                                 sma20>=lag(sma20), pos_col,neg_col),
                  group=1)#color="#d4af37"
              )+
  ggplot2::scale_color_identity()+
  ggplot2::geom_text(data=~. |> filter(date==max(date)|
                                  date==min(date)|
                                 close==max(close)|
                                  close==min(close)),
              aes(label=date, y=-Inf), color="white", size=2.5,#vjust=-1, 
              check_overlap = T)+
  ggplot2::geom_text(data=~. |> filter(#date==max(date)|
                                  #date==min(date)|
                                  close==max(close)|
                                  close==min(close)),
              aes(label=close, x=min(date)), color="white", size=2.5, check_overlap = T)+
  ggplot2::labs(y=NULL, x=NULL)+
  ggplot2::coord_cartesian(clip = "off")+
  jsalomon::theme_bors()+
  ggplot2::theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none")

coloring_ret<-\(x, pc=pos_col, nc=neg_col){
x<-ifelse(
  x>=0, 
  glue::glue("<span style='color:{pc}'>{x}%</span>"),
  glue::glue("<span style='color:{nc}'>{x}%</span>")
)

x
}
#coloring_ret(-10)

return_table<-df1 |> mutate(d=round( ((close/lag(close))-1)*100,2  ),
              d5=round( ((close/lag(close,5))-1)*100,2  ),
              d25=round( ((close/lag(close,25))-1)*100,2  ),
              d100=round( ((close/lag(close,100))-1)*100,2  ),
              d250=round( ((close/lag(close,250))-1)*100,2  )
              )  |> 
  filter(date==max(date)) |> 
  transmute(r=1,
            `1 Day`=coloring_ret(d),
         `5 Days`=coloring_ret(d5),
         `25 Days`=coloring_ret(d25),
         `100 Days`=coloring_ret(d100),
         `250 Days`=coloring_ret(d250)) |> 
  pivot_longer(cols = 2:6) |> 
  mutate(rn=row_number(),
    value=glue::glue("<span style='color:grey90'>{name}:</span> {value}"),
    name=fct_reorder(name,-rn)) |> 
  ggplot2::ggplot(aes(1, name))+
  ggtext::geom_richtext(aes(label=value), fill=NA, color=NA,size=2.5,
                        hjust=0.5)+
  labs(x=NULL,y=NULL)+
  theme_void()+
  ggplot2::theme(plot.background = element_rect(fill="#1e1e1e"))

  
  p<-  dd |>  ggplot2::ggplot(aes(x = date, y = close)) +
    ggplot2::geom_text(aes(label=ifelse(date==lubridate::floor_date(median(date)), paste0(symbol), NA), y=((max(close)-min(close))/2)+min(close)  ),
                       color="white",
                       size=26,
                       alpha=0.1
    )+
    ggplot2::geom_ribbon(aes(x = date, ymin= lower_bb, ymax = upper_bb),color="grey90",linewidth=0.3, lty=3, fill= "grey", alpha = 0.03)+
    
    tidyquant::geom_candlestick(aes(open = open, high = high, low = low, close = close),
                                colour_up   = pos_col  ,
                                colour_down = neg_col ,
                                fill_up   = pos_col ,
                                fill_down = neg_col,
                                alpha=0.9,
                                size=0.1
                                ) +
    ggplot2::geom_hline(aes(yintercept=ifelse(date==max(date), close,NA)), color="cyan",lty=5,size=0.2)+
    ggplot2::geom_line(data=t,
                       aes(lubridate::ymd(date),pred_value, group=pred),
                       color="grey70",linewidth=0.2)+
    ggplot2::geom_line(data=l,
                       aes(lubridate::ymd(date),pred_value, group=pred),
                       color=scales::alpha("yellow",0.5),
                       linewidth=0.2)+
    geomtextpath::geom_textline(aes(y=sma10, label="10"),
                                size = 3, color = "pink",hjust = 0.2)+
    geomtextpath::geom_textline(aes(y=sma20, label="20"),
                                size = 3, color = "grey70",hjust = 0.2)+
    geomtextpath::geom_textline(aes(y=sma50, label="50"),
                                size = 3, color = "cyan",hjust = 0.2)+
    geomtextpath::geom_textline(aes(y=sma100, label="100"),
                                size = 3, color = "blue",hjust = 0.2)+
    geomtextpath::geom_textline(aes(y=sma200, label="200"),
                                size = 3, color = "red",hjust = 0.2)+
    geomtextpath::geom_textline(aes(y=ema10, label="ema10"),
                                size = 2, color = "#D8315B",hjust = 0.2)+
    geomtextpath::geom_textline(aes(y=ema21, label="ema21"),
                                size = 2, color = "#3E92CC",hjust = 0.2)+
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
    ggplot2::scale_y_continuous(limits = c(min(dd$low)*0.9,max(dd$high)*1.05),
                                sec.axis = sec_axis( trans=~.))+
    bdscale::scale_x_bd(business.dates=dd$date, max.major.breaks=10, labels=scales::date_format("%b-'%y"))+
    labs(title =NULL,
         y=NULL, x = NULL) +
    coord_cartesian(clip = "off")+
    #scale_y_continuous(sec.axis = sec_axis( trans=~.))+
    jsalomon::theme_bors()+
    theme(axis.text.x=element_blank(),
          axis.title.x =element_blank()#,
          #plot.title.position = 
          #plot.title = element_text(vjust = (max(dd$close)-min(dd$close))*0.1*(-1) )
          )
  
  if(title_p==FALSE ){
    p<-p + ggplot2::theme(plot.title = element_blank())
    
  }
  if(title_p==TRUE){
    p<-p+geom_text(label= paste(ticker), x=-Inf,y=Inf, size=10, color="white",
                  vjust=1, hjust=0)
    
  }
  
  
  if(nrow(filter(dd, !is.na(tight)  ))>0   )  {
    p<-p+
      ggplot2::geom_point(aes(y=tight),shape=25, color=tight_col#, alpha=0.5
      )
  } 
  
  if(nrow(filter(dd, !is.na(tight_gain)  ))>0 ) { 
    p<-p  +
      # ggrepel::geom_text_repel(data=filter(dd,!is.na(tight_gain)),
      #   aes(label=paste0(tight_gain,"%\n(",days_hold,"d)"), 
      #       x=date, 
      #       y=tight
      #       )
      #   ,force_pull   = 0
      #   ,hjust=1
      #   ,point.padding = 0.2
      #   ,nudge_x = -4
      #   ,nudge_y = 5
      #   ,segment.size      = 0.2
    #   ,min.segment.length = 0
    #   , size=2.1
    #   ,segment.curvature = -0.01
    #   , direction = "y"
    #   ,color="green"
    # )
    ggplot2::geom_segment(aes(
      x= date,xend=date,
      y=ifelse(!is.na(tight_gain),high*1.05,NA),
      yend=high*1.05*((tight_gain*1.05/100)+1))
      ,color=tight_col
      , size=0.15 )+
      ggplot2::geom_text(aes(label=paste0("ADR: ",round(adr,1), "\n",tight_gain,"%\n(",days_hold,"d)"), x=date, y=high*1.055*((tight_gain*1.055/100)+1)  )
                         ,check_overlap = T, vjust="left", color=tight_col, size=2.1)
    
  }
  
  
  pz<-p+ggplot2::coord_cartesian(xlim = c(limit_date,max_date
  ),
  ylim = c( dplyr::filter(dd,date>=limit_date) |> dplyr::summarize(min_low=min(low)*0.95 ) |> dplyr::pull(min_low),
            dplyr::filter(dd,date>=limit_date) |> dplyr::summarize(max_high=max(high)*1.05 ) |> dplyr::pull(max_high) )
  
  )+
    labs(title =NULL,#subtitle =  paste0("Recent days"), 
         x=NULL, y=NULL)+
    theme(#axis.text.x=element_blank(),
      axis.title.x =element_blank(),
      plot.title = element_blank())
  
  if(title_p==FALSE ){
    pz<-pz + ggplot2::theme(plot.subtitle = element_blank())
    
  }  
  
  v<-dd |>  ggplot2::ggplot(aes(x = date, y = volume))+
    # ggplot2::geom_col(aes(fill=ifelse(close>dplyr::lag(close), "green", "red")))+
    ggplot2::geom_col(aes(fill=ifelse(close>dplyr::lag(close), pos_col, neg_col),
                          alpha=ifelse(volume>sma_vol_50, 0.9,0.3)
    ))+
    geom_vline(aes(xintercept=date[which.min(sma_vol_50)]), color="grey80",lty=1, size=0.2)+
    ggplot2::geom_line(aes(y=sma_vol_50,group=1, color=ifelse(sma_vol_50>dplyr::lag(sma_vol_50), "cyan", "orange")))+
    ggplot2::scale_alpha_identity()+
    ggplot2::scale_fill_identity()+
    ggplot2::scale_color_identity()+
    ggplot2::scale_shape_identity()+
    bdscale::scale_x_bd(business.dates=dd$date, max.major.breaks=10, labels=scales::date_format("%b-'%y"))+
    labs(x="",y=NULL#, caption = paste("Data: Yahoo! Finance. Accessed ",Sys.Date(),".",sep="")
    )+
    jsalomon::theme_bors()+
    theme(legend.position = "none",
          axis.text.y = element_blank(),
          axis.title.x =element_blank()
    )
  
  if(nrow(filter(dd, volume>sma_vol_50*2  ))>0 ){
    v<-v+ggplot2::geom_point(aes(y=ifelse(volume>sma_vol_50*2, volume*1.04 ,NA ),
                                 color=ifelse(close>dplyr::lag(close), "cyan", "orange"),
                                 shape=ifelse(volume>sma_vol_50*2 & volume<sma_vol_50*3, 1,20)
    ))
  }
  
  
  vz<- v+coord_cartesian(xlim = c(limit_date,max_date),
                         ylim = c( dplyr::filter(dd,date>=limit_date) |> dplyr::summarize(min_vol=min(volume)*0.95 ) |> dplyr::pull(min_vol),
                                   dplyr::filter(dd,date>=limit_date) |> dplyr::summarize(max_vol=max(volume)*1.05 ) |> dplyr::pull(max_vol) )
  )+
    labs(title = NULL, x=NULL, y=NULL#,caption = paste("@salojoh | Data: Yahoo! Finance. Accessed ",Sys.Date(),".",sep="")
    )+
    theme(axis.text.x=element_blank(),
          axis.title.x =element_blank() )
  
  
  
  layout <- "
AAAAAAADDD
AAAAAAADDD  
AAAAAAADDD
AAAAAAADDD
AAAAAAADDD
AAAAAAADDD
AAAAAAADDD
AAAAAAADDD
AAAAAAADDD
AAAAAAADDD
AAAAAAAEEE
AAAAAAAFFG
BBBBBBBFFG
CCCCCCCCCC
"
  pw<-patchwork::wrap_plots(p,v, dp,pz,vz,minichart,return_table, design = layout) 
  pw
  #patchwork::plot_annotation(p,theme(text = element_text('mono')))
}

#plot_stock5("AAPL", 400, title_p=FALSE)
