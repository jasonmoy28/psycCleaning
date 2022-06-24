#' Composite column
#'
#' The function will perform a rowise aggregation which then divided by the total number of columns. It ignores rows with any NA value (i.e, will use listwise deletion)
#' @param data dataframe
#' @param cols vector or tidyselect syntax or helpers. column(s) that need to be composite
#' @param composite_col_name character. default as 'composite_column'. the column name of the composite column
#'
#' @return
#' return a dataframe with a new column with the composite score
#' @export
#'
#' @examples
#' test_df = data.frame(col1 = c(1,2,3,4),col2 = c(1,2,3,4), col3 = c(1,2,NA,4))
#' composite_df = composite_score(data = test_df)
#'
#'
composite_score = function(data,
                           cols = tidyselect::everything(),
                           composite_col_name = 'composited_column') {
  
  composite_column = data %>%
    dplyr::select(!!enquo(cols)) %>%
    apply(., 1, function(x) {sum(x)/length(x)}) %>% 
    data.frame(composite_column = .)
  
  return_df = data %>%
    dplyr::bind_cols(composite_column) %>%
    dplyr::rename(!!enquo(composite_col_name) := 'composite_column') %>%
    dplyr::ungroup()
  
  return(return_df)
}
