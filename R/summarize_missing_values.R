#' Count the number of missing / `NA` values
#'
#' It counts the number of missing / `NA` values in each column.
#'
#' @param data data frame.
#' @param cols vector or tidyselect syntax or helpers. default is all columns
#' @param group character. count missing values by group. 
#' @param print printing the returned data frame.
#'
#' @return
#' return a data frame with the number of NA values of each columns 
#' @export
#'
#' @examples
#' df1 = data.frame(col1 = c(1,2,3),col2 = c(1,NA,3),col3 = c(1,2,NA))
#' summarize_missing_values(df1,everything())
#'
summarize_missing_values = function(data,
                                    cols = dplyr::everything(),
                                    group = NULL,
                                    print = FALSE) {
  cols = dplyr::enquo(cols)
  group = dplyr::enquo(group)
  return_df = data %>%
    dplyr::group_by(!!group) %>%
    dplyr::summarize(dplyr::across(!!cols, ~ sum(is.na(.))))
  
  if (nrow(return_df) == 1) {
    return_df = return_df %>% tidyr::pivot_longer(dplyr::everything())
  }
  
  if (print) {print(return_df)} 
  
  return(return_df)
}
