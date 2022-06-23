#' Recode items of a dataframe (support reverse code)
#'
#' @param data a dataframe
#' @param cols vector or tidyselect syntax or helpers. column(s) that need to be recoded
#' @param code_from vector. the order must match with vector for code_to
#' @param code_to vector. the order must match with vector for code_from
#' @param retain_code vector. the code to be retained. default is everything.
#' @param reverse_code logical. Default as F. If T, it will use psych::reverse_code
#'
#' @export
#'
#' @examples
#' df1 = data.frame(x = 1:5)
#' recode_item(df1, cols = x, reverse_code = TRUE)
#'
recode_item <- function(data,
                        cols,
                        code_from = NULL,
                        code_to = NULL,
                        reverse_code = FALSE,
                        retain_code = "all") {
  
  if (is.null(code_from) & reverse_code == FALSE & retain_code == 'all') {
    stop()
  }
  
  cols = enquo(cols)
  data = data %>% dplyr::mutate(dplyr::across(!!cols,as.numeric))

  if (reverse_code == T) {
    # reverse code
    if (all(retain_code == 'all')) {
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
  } else if (!all(retain_code == 'all')){
    # only code to NA 
    return_df = data %>%
      dplyr::mutate(dplyr::across(!!cols, ~ dplyr::if_else(. %in% retain_code, ., NA_real_)))
  } 
  return_df = return_df %>% data.table::as.data.table() %>% tibble::as_tibble()
  return(return_df)
}
