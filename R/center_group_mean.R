#' Center with respect to group mean
#'
#' Center all columns with respect to the group mean. 
#' @param data data frame. 
#' @param cols vector or tidyselect syntax or helpers. column(s) that need to be centered.  Recommend using where(is.numeric) to exclude changing factors.
#' @param group character. grouping variable
#'
#' @return
#' return a data frame with the columns centered (replace existing columns)
#' @export
#'
#' @examples
#' center_group_mean(iris,where(is.numeric), group = Species)
#'

center_group_mean = function(data, cols, group){
  cols = enquo(cols)
  group = enquo(group)
  return_df = data %>%
    dplyr::group_by(dplyr::across(!!group)) %>%
    dplyr::mutate(dplyr::across(!!cols, function(x) { (x - mean(x,na.rm = TRUE))})) %>%
    dplyr::ungroup()
  return(return_df)
}
