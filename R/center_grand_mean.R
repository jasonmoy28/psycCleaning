#' Center with respect to grand mean
#'
#' Center all columns with respect to the grand mean
#' @param data dataframe
#' @param cols vector or tidyselect syntax or helpers. column(s) that need to be centered. Recommend using where(is.numeric) to exclude changing factors.
#' @param keep_original default is `FALSE`. Set to `TRUE` to keep original columns
#' 
#' @return
#' return a dataframe with the columns centered (replace existing columns)
#' @export
#'
#' @examples
#' center_grand_mean(iris,where(is.numeric))
#'
center_grand_mean = function(data,cols,keep_original=TRUE) {
  cols = enquo(cols)
  original_df = data %>% dplyr::select(!!cols)
  return_df = data %>%
    dplyr::mutate(dplyr::across(!!cols, function(x) { (x - mean(x,na.rm = TRUE))})) %>% 
    dplyr::rename_with(~ paste(.,'_grand_c',sep = ''),!!cols)
  if (keep_original == TRUE) {
    return_df = dplyr::bind_cols(return_df,original_df)
  }
  return(return_df)
}
