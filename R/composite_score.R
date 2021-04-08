#' Composite column
#'
#' The function will perform a rowise aggregation which then divided by the total number of columns. It ignores rows with any NA value (i.e, will use listwise deletion)
#' @param data dataframe
#' @param cols vector or tidyselect syntax or helpers. column(s) that need to be composite
#' @param composite_col_name character. default as 'composited_column'. the column name of the composite column
#'
#' @return
#' return a dataframe with a new column with the composite score
#' @export
#'
#' @examples
#' test_df = data.frame(col1 = c(1,2,3,4),col2 = c(1,2,3,4), col3 = c(1,2,NA,4))
#' composite_score(test_df, everything()) # as you can see row 3 was dropped
#'
composite_score = function(data, cols, composite_col_name = 'composited_column'){
  cols = ggplot2::enquo(cols)
  # get the number of columns
  ncols = data %>% dplyr::select(!!cols) %>% ncol()
  col_names = data %>% dplyr::select(!!cols) %>% names(.)

  df_NA = data %>%
    dplyr::filter(dplyr::across(!!cols, ~ is.na(.)))

  return_df = data %>%
    dplyr::filter(dplyr::across(!!cols, ~ !is.na(.))) %>%
    dplyr::mutate(sum = rowSums(dplyr::across(!!cols),na.rm = T)/ncols) %>%
    dplyr::bind_rows(df_NA)

  return_df[composite_col_name] = return_df['sum']
  return_df = return_df %>% dplyr::select(-'sum')
  return(return_df)
}
