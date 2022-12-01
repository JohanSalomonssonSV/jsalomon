#' fishnet_plot
#' @param symbol ticker
#' @param start_date start_date
#' @param end_date end_date
#' @param save_plot save_plot T/F
#' @importFrom tidyquant geom_candlestick
#' @importFrom ggplot2 ggplot geom_tile geom_col geom_label labs theme_minimal geom_line scale_fill_identity geom_vline geom_hline scale_color_identity geom_segment scale_alpha_identity scale_fill_manual
#' @importFrom dplyr mutate select filter  
#' @importFrom scales date_format
#' @importFrom bdscale scale_x_bd
#' @importFrom purrr set_names map_dfc
#' @importFrom roll roll_mean
#' @importFrom tidyr pivot_longer
#' @importFrom here here
#' @importFrom grid grid.text gpar
#' @export
#' @return p
#' 

fishnet_plot<-function(symbol="^GSPC", start_date="2021-01-01", end_date=lubridate::today(), save_plot=TRUE){
  
  df<-jsalomon::getstk(symbol, start_date = lubridate::ymd(start_date)-320, end_date = end_date)   
  df<- df |> dplyr::filter(!is.na(close)) |> 
    mutate(sma_vol_50=roll::roll_mean(volume, width = 50))
  fishnet<-function(close=close){
    
    t<-purrr::map_dfc(c(seq(10, 200,3),200), function(x){ 
      roll::roll_mean(close,x )  
      
    })
    t<-t |> purrr::set_names(paste0("sma",c(seq(10, 200,3),200) ))
    t
  }
  
  
  plot_fishnet<-function(start_date){
    smas<-dplyr::bind_cols(date=df$date,fishnet(df$close)) |> 
      tidyr::pivot_longer(cols = 2:66, values_to = "close") |> 
      dplyr::filter(date>=start_date)
    
    plot_data<-df |> 
      dplyr::filter(date>=start_date)
    
    pp<-plot_data |> 
      ggplot2::ggplot(aes(date,close))+
      # ggplot2::geom_text(label=str_remove(symbol,".ST"), x=-Inf, y=Inf
      #                    
      #                    
      #                    ,color="grey20",
      #                    size=15,
      #                    hjust="inward",
      #                    vjust="inward",
      #                    alpha=0.7)+
      annotation_custom(grid::grid.text(paste0(str_remove(symbol,".ST")),
                                       #color="grey20",
                                       #size=15,
                                       #alpha=0.7, 
                                       gp=grid::gpar(fontsize=180, col="grey20")
                                       ), 
                        xmin = -Inf, 
                        xmax = Inf,
                        ymin = -Inf, 
                        ymax = Inf
                        
                        )+
      # ggplot2::geom_text(aes(label=ifelse(date==lubridate::floor_date(median(date, na.rm = T)), paste0(str_remove(symbol, ".ST")), NA), y=((max(close)-min(close))/2)+min(close)  ),
      #                    color="white",
      #                    size=20,
      #                    alpha=0.1
      # )+
      #geom_line()+
      ggplot2::geom_line(data = smas, aes(date, close, group=name#, color=ifelse(adjusted<=lag(adjusted),"grey80","red" )
      ), color="grey90", 
      size=0.1)+
      #geom_line(color="cyan")+
      tidyquant::geom_candlestick(aes(open = open, high = high, low = low, close = close),
                                  colour_up   = "#648DE5"  ,
                                  colour_down = "#BC5D2E" ,
                                  fill_up   = "#7C9BDD"  ,
                                  fill_down = "#F34213" )+
      ggplot2::geom_hline(aes(yintercept=ifelse(date==max(date), close,NA)), color="cyan",lty=5,size=0.1)+
      ggplot2::geom_label(data=filter(plot_data,date==max(date)),
                          aes(label=round(close,2),
                              #y=close, 
                              x=min(plot_data$date) 
                          ),
                          #hjust=-.1,
                          fill="black",
                          size=2.5,
                          color="cyan"
      )+
      #scale_color_identity()+
      scale_y_continuous(#labels = scales::dollar
        sec.axis = sec_axis( trans=~.)
                         )+
      bdscale::scale_x_bd(business.dates=plot_data$date, 
                          max.major.breaks=10, 
                          labels=scales::date_format("%b-'%y"))+
      ggplot2::labs(#title=symbol,
        x="", y=""
      )+
      jsalomon::theme_bors()+
      ggplot2::theme(legend.position = "none",
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     axis.title.x = element_blank(),
                     axis.text.x = element_blank(),
                     text = element_text(family = "sans")
      )
    
    
    v<-
      plot_data |> #dplyr::filter(date>=start_date) |>
      ggplot2::ggplot(aes(x = date, y = volume))+
      # ggplot2::geom_col(aes(fill=ifelse(close>dplyr::lag(close), "green", "red")))+
      ggplot2::geom_col(aes(fill=ifelse(close>dplyr::lag(close), "#648DE5", "#BC5D2E"),
                            alpha=ifelse(volume>sma_vol_50, 0.9,0.3)
      ))+
      ggplot2::geom_vline(aes(xintercept=date[which.min(sma_vol_50)]), color="grey80",lty=1, size=0.2)+
      ggplot2::geom_line(aes(y=sma_vol_50,group=1, color=ifelse(sma_vol_50>dplyr::lag(sma_vol_50), "cyan", "orange")))+
      ggplot2::scale_alpha_identity()+
      ggplot2::scale_fill_identity()+
      ggplot2::scale_color_identity()+
      #ggplot2::scale_shape_identity()+
      ggplot2::geom_hline(aes(yintercept=ifelse(date==max(date), volume,NA)), color="cyan",lty=5,size=0.1)+
      bdscale::scale_x_bd(business.dates=df$date, max.major.breaks=10, labels=scales::date_format("%b-'%y"))+
      labs(x="",y=NULL, 
           caption = paste0("Data: Yahoo Finance, ", lubridate::today(), "\n@stops2breakeven"))+
      jsalomon::theme_bors()+
      theme(legend.position = "none",
            axis.text.y = element_blank(),
            axis.title.x =element_blank()
      )
    
    
    layout <- "
AAAAAA
AAAAAA 
AAAAAA
AAAAAA
AAAAAA
AAAAAA
AAAAAA
AAAAAA
BBBBBB
BBBBBB
"
    p<-patchwork::wrap_plots(pp,v, design = layout) 
    p  
    
    
  }
  p<-plot_fishnet(start_date = start_date)  
  
  if (save_plot==TRUE){
    ggplot2::ggsave(here::here("github","avanza","fishnet_plots", paste0("fishnet_",symbol,".png")),plot=p,width = 12, height = 6, dpi = 300)
  }
  p
  
}


#fishnet_plot(symbol = "FNM.ST")
