#' scale_color_salo
#' @importFrom ggplot2 scale_color_manual
#' @export
#' @param theme theme JS
#' @param key list of colors

scale_color_salo <- function(theme="JS", key = list(
  JS=c("#4D6EDD",
       "#5FFB17",
       "#FF4D6B",
       "#FFD800",
       "#00E5E5",
       "#FF6C02",
       "#B47CFF",
       "#FF2020")#,
  #Bright=c("#FF0000","#00FF00","#FFFF00","#00FFFF","#FF00FF","#FF00FF")
)) {
  
  ggplot2::scale_color_manual(values=key[[theme]])
  
}