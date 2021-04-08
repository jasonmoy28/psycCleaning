#' Center with respect to grand mean
#'
#' Center all columns with respect to the grand mean
#' @param data dataframe
#' @param cols vector or tidyselect syntax or helpers. column(s) that need to be centered. Recommend using where(is.numeric) to exclude changing factors.
#'
#' @return
#' return a dataframe with the columns centered (replace existing columns)
#' @export
#'
#' @examples
#' center_grand_mean(iris,where(is.numeric))
#'
center_grand_mean = function(data, cols) {
  cols = ggplot2::enquo(cols)
  return_df = data %>%
    dplyr::mutate(dplyr::across(!!cols, function(x) { (x - mean(x,na.rm = T))}))
  return(return_df)
}
