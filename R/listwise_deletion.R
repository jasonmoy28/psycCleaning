#' Listwise deletion
#'
#' Perform listwise deletion (the entire rows is disregarded if the row has one NA value)
#' @param data data frame
#' @param cols vector or tidyselect syntax or helpers. column(s) that need to perform listwise deletion. Default is all columns (i.e., `dplyr::everything()`)
#' return a data frame that has performed listwise deletion
#' @export
#'
#' @examples
#' test_df = data.frame(col1 = c(1,2,3),col2 = c(1,NA,3),col3 = c(1,2,NA))
#' listwise_deletion(test_df,col1:col2) # you can see that the row with NA in col3 is not deleted
#'
listwise_deletion = function(data, cols = dplyr::everything()) {
  return_df = data %>%
    dplyr::filter(dplyr::if_all(!!enquo(cols), ~ !is.na(.)))
  return(return_df)
}
