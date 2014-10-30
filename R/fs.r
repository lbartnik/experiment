# TODO make this a S4 class
# TODO then there will be no 'experiment' parameter for each
#      function
# TODO add namespace parameter (a subdirectory)

#' @export
experiment <- function (path, name)
{
  dir <- file.path(path, name)
  if (!file.exists(dir) && !create.dir(dir, recursive = T))
    stop('cannot access directory', call. = F)

  structure(list(path = path, name = name,
                 meta_cache = tempfile(fileext = '.rds')),
		    class = 'experiment')
}


#' @export
is_experiment <- function (obj)
{
  inherits(obj, 'experiment')
}


#' @export
#' @importFrom digest digest
add_result_set <- function (experiment, set)
{
  stopifnot(is_experiment(experiment))
  md5 <- digest(set, 'md5')
  dir <- file.path(experiment$path, experiment$name,
                   substr(md5, 1, 2), substr(md5, 3, 4))
  if (!file.exists(dir) && !create.dir(dir, recursive = T))
    stop('cannot create cache directory', call. = F)
  saveRDS(set, file.path(dir, paste0(md5, '.rds')))
}


#' @export
list_files <- function (experiment)
{
  stopifnot(is_experiment(experiment))
  dir <- file.path(experiment$path, experiment$name)
  file.path(dir, list.files(dir))
}


#' @importFrom plyr adply
build_meta_cache <- function (experiment, names)
{
  adply(list_files(experiment), 1, function (file_name) {
    x <- readRDS(file_name)
    unique(select_(x, .dots = names))
  }) %>%
    rename(file_name = X1)
}


find_files <- function (experiment, crt_list)
{
  if (!file.exists(experiment$meta_cache)) {
    cache <- build_meta_cache(experiment, names(crt_list))
    saveRDS(cache, file = experiment$meta_cache)
  }
  else
    cache <- readRDS(experiment$meta_cache)

  # TODO this won't work with list; lazyeval is required
  #      and == instead of = which is in a list experession
  filter_(cache, crt_list)$file_name
}


#' @export
find_result_sets <- function (experiment, crt_list)
{
  adply(find_files(experiment, crt_list), 1, function(file_name) {
    readRDS(file_name)
  })
}

