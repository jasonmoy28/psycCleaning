#' Listwise deletion
#'
#' Perform listwise deletion (the entire rows is disregarded if the row has one NA value)
#' @param data dataframe
#' @param cols vector or tidyselect syntax or helpers. column(s) that need to be centered
#'
#' @return
#' return a dataframe with listwise deletion
#' @export
#'
#' @examples
#' test_df = data.frame(col1 = c(1,2,3),col2 = c(1,NA,3),col3 = c(1,2,NA))
#' listwise_deletion(test_df,col1:col2) # you can see that the row with NA in col3 is not deleted
#'
listwise_deletion = function(data, cols) {
  cols = ggplot2::enquo(cols)
  return_df = data %>%
    dplyr::filter(dplyr::across(!!cols, ~ !is.na(.)))
  return(return_df)
}
