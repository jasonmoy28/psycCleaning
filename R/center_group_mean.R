#' Center with respect to group mean
#'
#' This function will compute group-mean-centered scores.
#' 
#' @param data A data.frame or a data.frame extension (e.g. a tibble).
#' @param cols Columns that need to be centered. See `dplyr::dplyr_tidy_select` for available options. 
#' @param group character. grouping variable
#' @param keep_original default is `TRUE`. Set to `FALSE` to remove original columns
#' 
#' @return
#' An object of the same type as .data. The output has the following properties:  
#' 1. Columns from .data will be preserved
#' 2. Columns with scores that are group-mean centered
#' 
#' @export
#'
#' @examples
#' center_group_mean(iris,where(is.numeric), group = Species)
#'

center_group_mean = function(data,cols,group,keep_original=TRUE){
  cols = enquo(cols)
  group = enquo(group)
  
  group_num = data %>% dplyr::select(!!enquo(group)) %>% names %>% length()
  if (group_num == 0) {
   stop('Group variable need to be specified') 
  }
  original_df = data %>% dplyr::select(!!cols)
  return_df = data %>%
    dplyr::group_by(dplyr::across(!!group)) %>%
    dplyr::mutate(dplyr::across(!!cols, function(x) { (x - mean(x,na.rm = TRUE))})) %>% 
    dplyr::rename_with(~ paste(.,'_group_c',sep = ''),!!cols) %>% 
    dplyr::ungroup()
  if (keep_original == TRUE) {
    return_df = dplyr::bind_cols(return_df,original_df)
  }
  return(return_df)
}
center_group_mean(iris,where(is.numeric),group = Species)
