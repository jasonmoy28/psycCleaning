#' Listwise deletion
#'
#' Perform listwise deletion (the entire rows is disregarded if the row has one `NA` value)
#' 
#' @param data A data.frame or a data.frame extension (e.g. a tibble).
#' @param cols Columns that need to use listwise deletion. See `dplyr::dplyr_tidy_select` for available options. 
#' @return 
#' An object of the same type as .data with rows revmoed if the row has one `NA` value
#' 
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
