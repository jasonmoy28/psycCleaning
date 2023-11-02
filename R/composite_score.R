#' Composite column
#'
#' The function will perform a row-wise aggregation which then divided by the total number of columns. 
#' 
#' @param data A data.frame or a data.frame extension (e.g. a tibble).
#' @param cols Columns that need to be composited See `dplyr::dplyr_tidy_select` for available options.
#' @param composite_col_name Name for the new composited columns. Default is 'composite_column'.
#' @param na.rm  Ignore NA. The default is `FALSE`. If set to `TRUE`, the composite score will be `NA` if there is one or more `NA` in any of the columns.
#'
#' @return
#' An object of the same type as .data. The output has the following properties:
#' 1. Columns from .data will be preserved.
#' 2. Columns with composited scores.
#' @export
#'
#' @examples
#' test_df = data.frame(col1 = c(1,2,3,4),col2 = c(1,2,3,4), col3 = c(1,2,NA,4))
#' composite_df = composite_score(data = test_df)
#'
#'
composite_score = function(data,
                           cols = dplyr::everything(),
                           na.rm = FALSE,
                           composite_col_name = 'composited_column') {
  
  composite_column = data %>%
    dplyr::select(!!enquo(cols)) %>%
    apply(., 1, function(x) {mean(x,na.rm = na.rm)}) %>% 
    data.frame(composite_column = .)
  
  return_df = data %>%
    dplyr::bind_cols(composite_column) %>%
    dplyr::rename(!!enquo(composite_col_name) := 'composite_column') %>%
    dplyr::ungroup()
  
  return(return_df)
}
