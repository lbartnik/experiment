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
    stop("id '", id, "' not found in storage", call. = FALSE)
  }
  readRDS(path)
}

restore_object <- function (st, id) restore_file(st, id, '.rds')

restore_tags <- function (st, id) restore_file(st, id, '_tags.rds')



#' @importFrom lazyeval lazy_eval
#' @importFrom stringi stri_replace_last_fixed
#' 
restore_by <- function (st, what, dots)
{
  stopifnot(what %in% c('tags', 'objects'))
  
  paths <- sort(list.files(st$path, "_tags.rds$", full.names = T, recursive = T))
  tgs <- lapply(paths, function (file) {
    tags <- readRDS(file)
    if (all(unlist(lazy_eval(dots, tags))))
      return(tags)
  })
  
  idx <- !vapply(tgs, is.null, logical(1))
  ids <- stri_replace_last_fixed(basename(paths[idx]), '_tags.rds', '')

  if (what == 'tags')
    return(`names<-`(tgs[idx], ids))
  
  paths <- stri_replace_last_fixed(paths[idx], '_tags.rds', '.rds')
  objs <- lapply(paths, readRDS)
  `names<-`(objs, ids)
}



#' @importFrom lazyeval lazy_dots
#'
restore_objects_by <- function (st, ...)
{
  dots <- lazy_dots(...)
  restore_by(st, 'objects', dots)
}


#' @importFrom lazyeval lazy_dots
#'
restore_tags_by <- function (st, ...)
{
  dots <- lazy_dots(...)
  restore_by(st, 'tags', dots)
}


count_objects <- function (st)
{
  length(list.files(st$path, "[^s].rds$", recursive = TRUE))
}





