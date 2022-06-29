#' Count number of non-missing / non-NA values
#'
#' It counts the number of non-missing / non-NA values. You can pass everything() to cols in order to get all the missing value for all columns
#'
#' @param data data frame. 
#' @param cols vector or tidyselect syntax or helpers. default is all columns.
#' @param group character. If specify, the return data frame would show the number of missing values of each column by group. 
#' @param print printing the return data frame.
#'
#' @return
#' return a data frame with the number of non-NA values of each column. 
#' @export
#'
#' @examples
#' test_df = data.frame(col1 = c(1,2,3),col2 = c(1,NA,3),col3 = c(1,2,NA))
#' summarize_non_missing_values(test_df,everything())
#'
summarize_non_missing_values = function(data,
                                        cols = dplyr::everything(),
                                        group = NULL,
                                        print = FALSE) {
  cols = dplyr::enquo(cols)
  group = dplyr::enquo(group)
  return_df = data %>%
    dplyr::group_by(!!group) %>%
    dplyr::summarize(dplyr::across(!!cols, ~ sum(!is.na(.))))
  
  if (nrow(return_df) == 1) {
    return_df = return_df %>% tidyr::pivot_longer(dplyr::everything())
  }
  
  if (print) {
    print(return_df)
  }
  return(return_df)
}
