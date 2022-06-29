#' Re-code values of a data frame (support reverse code)
#'
#' @param data a data frame
#' @param cols vector or tidyselect syntax or helpers. column(s) that need to be re-coded
#' @param code_from vector. the order must match with vector for `code_to`
#' @param code_to vector. the order must match with vector for `code_from`
#' @param retain_code vector. Specify the values to be retain 
#' @param reverse_code logical. Default as `FALSE`. If `TRUE`, it will reverse code the value of that column. 
#'
#' @return 
#' return a data frame  with values that has been recoded
#' 
#' @export
#' @examples
#' pre_recoded_df = tibble::tibble(x1 = 1:5, x2 = 5:1, x3 = 6:10, y = 1:5)
#' recoded_df = recode_item(pre_recoded_df, cols = dplyr::contains('x'), reverse_code = TRUE)
#' list(pre_recoded_df = pre_recoded_df,recoded_df = recoded_df)
#' 
recode_item <- function(data,
                        cols,
                        code_from = NULL,
                        code_to = NULL,
                        reverse_code = FALSE,
                        retain_code = NULL) {
  
  if (is.null(code_from) & reverse_code == FALSE & is.null(retain_code)) {
    stop()
  }
  
  cols = enquo(cols)
  data = data %>% dplyr::mutate(dplyr::across(!!cols,as.numeric))

  if (reverse_code == TRUE) {
    # reverse code
    if (is.null(retain_code)) {
      # if retain all code
      return_df = data %>%
        dplyr::mutate(dplyr::across(!!cols, ~ psych::reverse.code(-1,.)))

    } else{
      # if retain only selected code
      return_df = data %>%
        dplyr::mutate(dplyr::across(!!cols, ~ dplyr::if_else(. %in% retain_code, ., NA_real_))) %>%
        dplyr::mutate(dplyr::across(!!cols, ~ psych::reverse.code(-1,.)))
    }

  } else if (!is.null(code_from)){
    # use code from and code to
    names(code_to) = code_from
    return_df = data %>% 
      dplyr::mutate(dplyr::across(!!cols, function(lamda) {dplyr::recode(lamda,!!!code_to)}))
  } else if (!is.null(retain_code)){
    # only code to NA 
    return_df = data %>%
      dplyr::mutate(dplyr::across(!!cols, ~ dplyr::if_else(. %in% retain_code, ., NA_real_)))
  } 
  return_df = return_df %>% data.table::as.data.table() %>% tibble::as_tibble()
  return(return_df)
}
