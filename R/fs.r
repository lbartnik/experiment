# store any R object in a RDS file and access it by its hash code


#' Initialize/create a new/open existing collection.
#' 
#' @param path Path to the folder.
#' @param .create Create the collection if it is not under \code{path}.
collection <- function (path, .create = FALSE)
{
  if (!dir.exists(path)) {
    if (!.create)
      stop('collection does not exist but .create is FALSE', call. = FALSE)
    if (!dir.create(path, recursive = TRUE))
      stop('cannot create directory ', path, call. = FALSE)
  }
  
  structure(list(path = path), class = 'collection')
}


is_collection <- function (x) {
  inherits(x, 'collection')
}


#' Add an object to a collection.
#' 
#' @param cl Collection object.
#' @param obj An object to be added.
#' @return Invisibly a hash of \code{obj}.
#' 
add_object <- function (cl, obj)
{
  stopifnot(is_collection(cl))
  id <- hash(obj)
  path <- file.path(cl$path, make_path(id))
  
  # remove hash attribute before saving
  if (!file.exists(path))
    saveRDS(`attr<-`(obj, hash_attribute_name, NULL), path)
  
  invisible(id)
}



make_path <- function (id) {
  file.path(substr(id, 0, 2), substr(id, 2, 4), paste0(id, '.rds'))
}


