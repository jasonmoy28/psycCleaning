#' Z scored with with respect to the grand mean
#'
#' Z-scored that uses the grand mean in the z-score formula
#' @param data dataframe
#' @param cols vector or tidyselect syntax or helpers. column(s) that need to be centered
#'
#' @return
#' retrun a dataframe with the columns z-scored (replace existing columns)
#' @export
#'
#' @examples
#' z_scored_grand_mean(iris,where(is.numeric))
#'
z_scored_grand_mean = function(data, cols) {
  cols = enquo(cols)
  return_df = data %>%
    dplyr::mutate(dplyr::across(!!cols, function(x) { (x - mean(x,na.rm = T))/stats::sd(x,na.rm = T)}))
  return(return_df)
}
