#' Z scored with with respect to the grand mean
#'
#' Z-scored that uses the grand mean in the z-score formula
#' @param data dataframe
#' @param cols vector or tidyselect syntax or helpers. column(s) that need to be centered
#' @param keep_original default is `FALSE`. Set to `TRUE` to keep original columns
#'
#' @return
#' return a dataframe with the columns z-scored (replace existing columns)
#' @export
#'
#' @examples
#' z_scored_grand_mean(iris,where(is.numeric))
#'
z_scored_grand_mean = function(data,cols,keep_original=TRUE) {
  cols = enquo(cols)
  original_df = data %>% dplyr::select(!!cols)
  return_df = data %>%
    dplyr::mutate(dplyr::across(!!cols, function(x) { (x - mean(x,na.rm = TRUE))/stats::sd(x,na.rm = TRUE)})) %>% 
    dplyr::rename_with(~ paste(.,'_grand_z',sep = ''),!!cols)
  if (keep_original == TRUE) {
      return_df = dplyr::bind_cols(return_df,original_df)
  }
  return(return_df)
}
