# store any R object in a RDS file and access it by its hash code


#' Initialize/create a new/open existing storage.
#' 
#' @param path Path to the folder.
#' @param .create Create the storage if it is not under \code{path}.
storage <- function (path, .create = FALSE)
{
  if (!dir.exists(path)) {
    if (!.create)
      stop('storage does not exist but .create is FALSE', call. = FALSE)
    if (!dir.create(path, recursive = TRUE))
      stop('cannot create directory ', path, call. = FALSE)
  }
  
  structure(list(path = path), class = 'storage')
}


is_storage <- function (x) {
  inherits(x, 'storage')
}


#' Add an object to a storage.
#' 
#' @param st Storage object.
#' @param id Add object under this identifier.
#' @param obj An object to be added.
#' @param tags Tags that describe \code{obj}.
#' @return Invisibly a hash of \code{obj}.
#' 
add_object <- function (st, id, obj, tags)
{
  stopifnot(is_storage(st))
  path <- file.path(st$path, make_path(id))
  
  # remove hash attribute before saving
  if (!file.exists(path)) {
    dir.create(dirname(path), recursive = T, showWarnings = T, mode = "0700")
    saveRDS(`attr<-`(obj, hash_attribute_name, NULL), paste0(path, '.rds'))
    saveRDS(tags, paste0(path, '_tags.rds'))
  }
  
  invisible(id)
}



make_path <- function (id) {
  file.path(substr(id, 1, 2), substr(id, 3, 4), paste0(id, '.rds'))
}


