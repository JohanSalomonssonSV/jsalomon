#' fishnet_plot
#' @param symbol ticker
#' @param start_date start_date
#' @param end_date end_date
#' @param save_plot save_plot T/F
#' @importFrom tidyquant geom_candlestick
#' @importFrom ggplot2 ggplot geom_abline geom_tile geom_col geom_label labs theme_minimal geom_line scale_fill_identity geom_vline scale_color_identity geom_segment scale_alpha_identity scale_fill_manual
#' @importFrom dplyr mutate select filter  
#' @importFrom scales date_format
#' @importFrom bdscale scale_x_bd
#' @importFrom purrr set_names map_dfc
#' @importFrom roll roll_mean
#' @importFrom tidyr pivot_longer
#' @importFrom here here
#' @export
#' @return p
#' 

fishnet_plot<-function(symbol="^GSPC", start_date="2021-01-01", end_date=lubridate::today(), save_plot=TRUE){
  
  df<-jsalomon::getstk(symbol, start_date = lubridate::ymd(start_date)-320, end_date = end_date)   
  df<- df |> dplyr::filter(!is.na(adjusted))  
  fishnet<-function(adjusted=adjusted){
    
    t<-purrr::map_dfc(c(seq(10, 200,3),200), function(x){ 
      roll::roll_mean(adjusted,x )  
      
    })
    t<-t |> purrr::set_names(paste0("sma",c(seq(10, 200,3),200) ))
    t
  }
  
  
  plot_fishnet<-function(start_date){
    smas<-dplyr::bind_cols(date=df$date,fishnet(df$adjusted)) |> 
      tidyr::pivot_longer(cols = 2:66, values_to = "adjusted") |> 
      dplyr::filter(date>=start_date)
    
    df |> 
      dplyr::filter(date>=start_date) |> 
      ggplot2::ggplot(aes(date,adjusted))+
      ggplot2::geom_text(label=str_remove(symbol,".ST"), x=-Inf, y=Inf,color="grey20",
                         size=15,
                         hjust="inward",
                         vjust="inward",
                         alpha=0.7)+
      # ggplot2::geom_text(aes(label=ifelse(date==lubridate::floor_date(median(date, na.rm = T)), paste0(str_remove(symbol, ".ST")), NA), y=((max(close)-min(close))/2)+min(close)  ),
      #                    color="white",
      #                    size=20,
      #                    alpha=0.1
      # )+
      #geom_line()+
      ggplot2::geom_line(data = smas, aes(date, adjusted, group=name#, color=ifelse(adjusted<=lag(adjusted),"grey80","red" )
      ), color="grey90", 
      size=0.1)+
      #geom_line(color="cyan")+
      tidyquant::geom_candlestick(aes(open = open, high = high, low = low, close = close),
                                  colour_up   = "#648DE5"  ,
                                  colour_down = "#BC5D2E" ,
                                  fill_up   = "#7C9BDD"  ,
                                  fill_down = "#F34213" )+
      #scale_color_identity()+
      #scale_y_continuous(labels = scales::dollar)+
      bdscale::scale_x_bd(business.dates=df$date, 
                          max.major.breaks=10, 
                          labels=scales::date_format("%b-'%y"))+
      ggplot2::labs(#title=symbol,
        x="", y="",
        caption = paste0("Data: Yahoo Finance, ", lubridate::today(), "\n@stops2breakeven")
      )+
      jsalomon::theme_bors()+
      ggplot2::theme(legend.position = "none",
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     axis.title.x = element_blank(),
                     text = element_text(family = "sans")
      )
  }
  p<-plot_fishnet(start_date = start_date)  
  
  if (save_plot==TRUE){
    ggplot2::ggsave(here::here("github","avanza","fishnet_plots", paste0("fishnet_",symbol,".png")),plot=p,width = 12, height = 6, dpi = 300)
  }
  p
  
}


#fishnet_plot(symbol = "SINCH.ST")
