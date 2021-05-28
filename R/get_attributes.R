#' Get Default Attributes
#'
#' Exposes the default general and variable attributes for writing metadata
#' template files.
#' @return list of default general and variable attribute data frames. The data
#'   frames include the attribute names and a description of the sort of
#'   information that should be included in the metadata.
#' @export
#'
#' @examples
#' a <- get_attributes()
#' a$gen_attributes
#' a$var_attributes
get_attributes <- function(){
  return(list(gen_attributes = gen_attrib,
              var_attributes = var_attrib))

}
