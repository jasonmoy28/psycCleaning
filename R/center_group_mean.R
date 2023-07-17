#' Center with respect to group mean
#'
#' Center all columns with respect to the group mean. 
#' @param data data frame. 
#' @param cols vector or tidyselect syntax or helpers. column(s) that need to be centered.  Recommend using where(is.numeric) to exclude changing factors.
#' @param group character. grouping variable
#' @param keep_original default is `FALSE`. Set to `TRUE` to keep original columns
#' 
#' @return
#' return a data frame with the columns centered (replace existing columns)
#' @export
#'
#' @examples
#' center_group_mean(iris,where(is.numeric), group = Species)
#'

center_group_mean = function(data,cols,group,keep_original=TRUE){
  cols = enquo(cols)
  group = enquo(group)
  original_df = data %>% dplyr::select(!!cols)
  return_df = data %>%
    dplyr::group_by(dplyr::across(!!group)) %>%
    dplyr::mutate(dplyr::across(!!cols, function(x) { (x - mean(x,na.rm = TRUE))})) %>% 
    dplyr::rename_with(~ paste(.,'_c',sep = ''),!!cols) %>% 
    dplyr::ungroup()
  if (keep_original == TRUE) {
    return_df = dplyr::bind_cols(return_df,original_df)
  }
  return(return_df)
}
