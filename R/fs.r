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



make_path <- function (id) {
  file.path(substr(id, 1, 2), substr(id, 3, 4), id)
}


#' Add an object to a storage.
#' 
#' @param st Storage object.
#' @param id Add object under this identifier.
#' @param obj An object to be added.
#' @param tags Tags that describe \code{obj}.
#' @return Invisibly a hash of \code{obj}.
#' 
store_object <- function (st, id, obj, tags = list())
{
  stopifnot(is_storage(st))
  stopifnot(is.list(tags))
  
  path <- file.path(st$path, make_path(id))
  
  # remove hash attribute before saving
  if (!file.exists(path)) {
    dir.create(dirname(path), recursive = T, showWarnings = F, mode = "0700")
    saveRDS(obj, paste0(path, '.rds'))
    saveRDS(tags, paste0(path, '_tags.rds'))
  }
  
  invisible(id)
}



restore_file <- function (st, id, ext)
{
  stopifnot(is_storage(st))
  path <- paste0(file.path(st$path, make_path(id)), ext)
  if (!file.exists(path)) {
    stop('object id not found in storage', call. = FALSE)
  }
  readRDS(path)
}

restore_object <- function (st, id) restore_file(st, id, '.rds')

restore_tags <- function (st, id) restore_file(st, id, '_tags.rds')



count_objects <- function (st)
{
  length(list.files(st$path, "[^s].rds$", recursive = TRUE))
}
