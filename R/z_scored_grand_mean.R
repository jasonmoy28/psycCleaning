#' Grand mean z-score
#' 
#' This function will compute z-scores with respect to the grand mean.
#' 
#' @param data A data.frame or a data.frame extension (e.g. a tibble).
#' @param cols Columns that need to be centered. See `dplyr::dplyr_tidy_select` for available options. 
#' @param keep_original default is `FALSE`. Set to `TRUE` to keep original columns
#'
#' @return
#' An object of the same type as .data. The output has the following properties:
#' 1. Columns from .data will be preserved
#' 2. Columns with scores that are z-scored 
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
    dplyr::rename_with(~ paste(.,'_z',sep = ''),!!cols)
  if (keep_original == TRUE) {
      return_df = dplyr::bind_cols(return_df,original_df)
  }
  return(return_df)
}
