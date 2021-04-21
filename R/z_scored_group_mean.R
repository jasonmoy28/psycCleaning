#' Z scored with with respect to the group mean
#'
#' Z-scored that uses the group mean in the z-score formula. Currently, it will drop the group column (Will try to fix this in the next version)
#' @param data dataframe
#' @param cols vector or tidyselect syntax or helpers. column(s) that need to be centered
#' @param group the grouping variable. If you need to pass multiple group variables, try to use quos(). Passing multiple group variables is not tested.
#'
#' @return
#' return a dataframe with the columns z-scored (replace existing columns)
#' @export
#'
#' @examples
#' z_scored_group_mean(iris, where(is.numeric), group = Species)
#' z_scored_group_mean(iris, 1:4, group = Species)
#'
z_scored_group_mean = function(data, cols, group) {
  cols = enquo(cols)
  group = enquo(group)
  return_df = data %>%
    dplyr::group_by(dplyr::across(!!group)) %>%
    dplyr::mutate(dplyr::across(!!cols, function(x) { (x - mean(x,na.rm = T))/stats::sd(x,na.rm = T)})) %>%
    dplyr::ungroup()
  return(return_df)
}

