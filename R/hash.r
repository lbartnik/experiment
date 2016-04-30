
#' Hash attribute name.
#' 
#' After each top-level expression succeedes, each object in the global
#' namespace is given this attribute whose value is the value returned
#' \link{\code{hash()}} invoked for that object.
#'   
#' @docType data
#' 
hash_attribute_name <- 'experiment::hash'


#' Computes hash for any object.
#' 
#' @importFrom digest digest
hash <- function (obj) {
  if (has_hash(obj))
    return(get_hash(obj))
  digest(obj, algo='md5')
}


has_hash <- function (x) hash_attribute_name %in% names(attributes(x))

get_hash <- function (x) attr(x, hash_attribute_name)
