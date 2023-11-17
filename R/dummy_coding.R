#' Dummy Coding
#' 
#' Create dummy-coded columns, supporting tidyselect syntax to process multiple columns simultaneously.
#' 
#' @param data data.frame object 
#' @param cols Columns that need to be dummy-coded See `dplyr::dplyr_tidy_select` for available options. 
#'
#' @return
#' An object of the same type as .data. The output has the following properties:
#' 1. Columns from .data will be preserved.
#' 2. Columns that are dummy-coded.
#' @export
#'
#' @examples
#' dummy_coding(iris,Species)
#' 
dummy_coding <- function(data,cols) {
  data = data %>% dplyr::mutate(dplyr::across(!!enquo(cols),~as.factor(.)))
  cols_names = data %>% dplyr::select(!!enquo(cols)) %>% names()
  
  return_df = data
  for (group in cols_names) {
    group_name = data %>%dplyr::select(!!enquo(group)) %>% names()
    distinct_group = data %>% dplyr::distinct(dplyr::across(!!enquo(group))) %>% dplyr::pull(.) %>% as.character() %>% .[!is.na(.)] 
    distinct_group_num = length(distinct_group)
    contrasts_df = data.frame(stats::contr.treatment(distinct_group_num,base = distinct_group_num))
    colnames(contrasts_df) = paste0(group_name,'_',distinct_group[1:nrow(contrasts_df)-1],'_dum')
    dummy_coded_df = contrasts_df %>% 
     dplyr::mutate(group = distinct_group) %>% 
     dplyr::rename(!!enquo(group):=group)
    return_df =dplyr::full_join(return_df,dummy_coded_df)
  }
  return(return_df)
}

