#' Count number of non-missing / non-NA values
#'
#' It counts the number of non-missing / non-NA values. You can pass everything() to cols in order to get all the missing value for all columns
#'
#' @param data dataframe
#' @param cols vector or tidyselect syntax or helpers. default is all columns 
#' @param group character. pass to group_by
#' @param print return the original data, and print the summary
#'
#' @return
#' return a dataframe with the number of non-NA values
#' @export
#'
#' @examples
#' test_df = data.frame(col1 = c(1,2,3),col2 = c(1,NA,3),col3 = c(1,2,NA))
#' summarize_non_missing_values(test_df,everything())
#'
summarize_non_missing_values = function(data, cols = dplyr::everything(), group = NULL,print = F) {
  cols = enquo(cols)
  if (!is.null(group)) {
    group = enquo(group)
    return_df = data %>%
      dplyr::group_by(!!group) %>%
      dplyr::summarize(dplyr::across(!!cols, ~ sum(!is.na(.))))
  } else {
    return_df = data %>%
      dplyr::summarize(dplyr::across(!!cols, ~ sum(!is.na(.)))) %>%
      tidyr::pivot_longer(tidyr::everything())
  }
  if (print) {
    print(return_df)
    return(data)
  } else {
    return(return_df)
  }
}
