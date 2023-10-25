#' Dummy Coding
#' Create dummy coding columns
#' @param data data.frame object 
#' @param cols vector or tidyselect syntax or helpers. column(s) that need to be re-coded
#'
#' @return
#' return a data.frame with factors being dummy coded
#' @export
#'
#' @examples
#' effect_coding(iris,Species)
#' Dummy Coding
#'
#' @param data data.frame object 
#' @param cols vector or tidyselect syntax or helpers. column(s) that need to be re-coded
#'
#' @return
#' return a data.frame with factors being effect coded
#' @export
#'
#' @examples
#' dummy_coding(iris,Species)
#' 
dummy_coding <- function(data,cols) {
  cols_names = data %>% select(!!enquo(cols)) %>% names()
  return_df = data
  for (group in cols_names) {
    group_name = data %>% select(!!enquo(group)) %>% names()
    distinct_group = data %>% distinct(across(!!enquo(group))) %>% pull(.) %>% as.character()
    distinct_group_num = data %>% distinct(across(!!enquo(group))) %>% nrow()
    contrasts_df = data.frame(contr.treatment(distinct_group_num,base = distinct_group_num))
    colnames(contrasts_df) = paste0(group_name,'_',distinct_group[1:nrow(contrasts_df)-1],'_dum')
    dummy_coded_df = contrasts_df %>% 
      mutate(group = distinct_group) %>% 
      rename(!!enquo(group):=group)
    return_df = full_join(return_df,dummy_coded_df)
  }
  return(return_df)
}

