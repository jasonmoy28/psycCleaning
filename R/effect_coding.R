#' Effect Coding
#'
#' @param data data.frame object 
#' @param cols vector or tidyselect syntax or helpers. column(s) that need to be re-coded
#'
#' @return
#' return a data.frame with factors being effect coded
#' @export
#'
#' @examples
#' effect_coding(iris,Species)
effect_coding = function(data,cols){
  names = data %>% dplyr::select(!!enquo(cols)) %>% colnames(.)
  return_df = data %>% dplyr::mutate(dplyr::across(dplyr::all_of(names),~as.factor(.)))
  for (name in names) {
    stats::contrasts(return_df[[name]]) = stats::contr.sum(length(levels(return_df[[name]])))
  }
  return(return_df)
}

