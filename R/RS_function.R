#' RS_function
#' @param tv_data tv_data 
#' @param col performance column
#' @param industry_rank t/F
#' @importFrom dplyr arrange mutate select group_by summarize left_join row_number
#' @export
#' @return data
#' 
RS_function<-function(tv_data,col=perf_ytd, industry_rank=FALSE){
  data<-tv_data |> dplyr::arrange({{col}}) |> 
    dplyr::mutate(rank_y=dplyr::row_number(),
           RS=round((rank_y/max(rank_y) ) *100,1)) |> 
    dplyr::select(-rank_y)
  if (industry_rank==TRUE){
    sec_rank<-data |> 
      dplyr::group_by(industry) |> 
      dplyr::summarize(ind_perf=median({{col}})) |> 
      dplyr::arrange(-ind_perf) |> 
      dplyr::mutate(ind_rank=dplyr::row_number() ) |> 
      select(-ind_perf)
    data<-data |> dplyr::left_join(sec_rank)
  }
  
  data
}