#' Z scored with with respect to the group mean
#'
#' Z-scored that uses the group mean in the z-score formula. Currently, it will drop the group column (Will try to fix this in the next version)
#' @param data dataframe
#' @param cols vector or tidyselect syntax or helpers. column(s) that need to be centered
#' @param group the grouping variable. If you need to pass multiple group variables, try to use quos(). Passing multiple group variables is not tested.
#' @param keep_original default is `FALSE`. Set to `TRUE` to keep original columns
#' 
#' @return
#' return a dataframe with the columns z-scored (replace existing columns)
#' @export
#'
#' @examples
#' z_scored_group_mean(iris, where(is.numeric), group = Species)
#' z_scored_group_mean(iris, 1:4, group = Species)
#'
z_scored_group_mean = function(data,cols,group,keep_original=TRUE) {
  cols = enquo(cols)
  group = enquo(group)
  original_df = data %>% dplyr::select(!!cols)
  
  return_df = data %>%
    dplyr::group_by(dplyr::across(!!group)) %>%
    dplyr::mutate(dplyr::across(!!cols, function(x) { (x - mean(x,na.rm = TRUE))/stats::sd(x,na.rm = TRUE)})) %>% 
    dplyr::rename_with(~ paste(.,'_z',sep = ''),!!cols) %>% 
    dplyr::ungroup()
  if (keep_original == TRUE) {
    return_df = dplyr::bind_cols(return_df,original_df)
  }
  return(return_df)
}