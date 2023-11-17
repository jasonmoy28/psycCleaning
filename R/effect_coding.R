#' Effect Coding
#' 
#' Create effect-coded columns, supporting tidyselect syntax to process multiple columns simultaneously. 
#'
#' @param data A data.frame or a data.frame extension (e.g. a tibble).
#' @param cols Columns that need to be effect-coded. See `dplyr::dplyr_tidy_select` for available options. 
#' @param factor The default is `FALSE`. If factor is set to `TRUE`, this function returns a tibble with effect-coded factors. If factor is set to `FALSE`, this function returns a tibble with effect-coded columns.
#' 
#' @return 
#' An object of the same type as .data. The output has the following properties:
#' 1. Columns from .data will be preserved.
#' 2. Columns that are effect-coded.
#' @export
#'
#' @examples
#' effect_coding(iris,Species)
#' 
effect_coding = function(data,cols,factor = FALSE){
  data = data %>% dplyr::mutate(dplyr::across(!!enquo(cols),~as.factor(.)))
  if (factor == TRUE) {
    names = data %>% dplyr::select(!!enquo(cols)) %>% colnames(.)
    return_df = data %>% dplyr::mutate(dplyr::across(dplyr::all_of(names),~as.factor(.)))
    for (name in names) {
      stats::contrasts(return_df[[name]]) = stats::contr.sum(length(levels(return_df[[name]])))
    }
    return(return_df) 
  } else if(factor == FALSE){
    cols_names = data %>% dplyr::select(!!enquo(cols)) %>% names()
    return_df = data
    for (group in cols_names) {
      group_name = data %>% dplyr::select(!!enquo(group)) %>% names()
      distinct_group = data %>% dplyr::distinct(dplyr::across(!!enquo(group))) %>% dplyr::pull(.) %>% as.character() %>% .[!is.na(.)] 
      distinct_group_num = length(distinct_group)
      contrasts_df = data.frame(stats::contr.sum(distinct_group_num))
      colnames(contrasts_df) = paste0(group_name,'_',distinct_group[1:nrow(contrasts_df)-1],'_eff')
      effect_coded_df = contrasts_df %>% 
       dplyr::mutate(group = distinct_group) %>% 
       dplyr::rename(!!enquo(group):=group)
      return_df =dplyr::full_join(return_df,effect_coded_df)
    }
    return(return_df)
    
  }
}

