#' theme_bors
#' @importFrom ggplot2 theme_classic theme element_text element_blank element_line element_rect update_geom_defaults
#' @export
#' @return A theme element
#' @param base_size base_size
#' @param plotbq plot background color
#' @param legendposition legend position

theme_bors<- function(base_size=2.5, plotbq="#040926",#"#1e1e1e", 
                      legendposition="none") {
  
   theme_minimal()+
    theme(#panel.background = element_rect(fill = "#313130"#, color = "transparent"),
      plot.background = element_rect(fill=plotbq, color=plotbq),
      strip.text = element_text(color="white", face = "bold", size=10),
      panel.grid = element_line(color = "#333333", size = 0.2),        
      panel.grid.major = element_line(color = "#333333",  size = 0.2),
      panel.grid.minor = element_line(color = "#333333", size = 0.15),
      axis.text = element_text(color = "#e0e0e0"),
      line = element_line(color = "#e0e0e0"),
      #axis.ticks = element_line(color = "#e0e0e0"),
      axis.ticks = element_blank(),
      legend.position=legendposition,
      title = element_text(color = "white"),
      plot.title = element_text(color = "white")
      
    )
  
}