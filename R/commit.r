
# --- interactive part of keeping track of commits ---------------------

#' Set tracking mode to ON/OFF.
#' 
#' @export
#' 
tracking_on <- function () {
  state$tracking <- TRUE
  update_prompt(state$tracking)
}


#' @name tracking_on
#' @export
tracking_off <- function () {
  state$tracking <- FALSE
  update_prompt(state$tracking)
}


#' Updates the prompt according to \code{state$tracking}.
#' 
update_prompt <- function (on_off) {
  if (on_off)
    options(prompt = "tracking > ")
  else
    options(prompt = "not tracking > ")
}



# Creating a new commit/checkout:
#
#  1. get the last commit
#  2. get the list of object names and see if any of them overlap with
#     the last commit
#  3. those that overlap we already have hashed in the last commit
#  4. those that have the same names but new contents need to be hashed
#  5. all new objects need to be hashed
#  6. put in the new commit:
#       - all names and their hashes
#       - the hash (id) of the last commit
#       - the contents of the history line (command) associated with creating
#         this new commit
#     (do we create a commit if none of the objects were changed? if the
#      history line is just printing something or opening help? rather not)
#  7. put the commit in the storage space
#  8. store all new objects and the commit object in the storage space

#' A callback run after each top-level expression is evaluated.
#'  
#' From \link{\code{addTaskCallback}}: if the data argument was
#' specified in the call to addTaskCallback, that value is given as the
#' fifth argument.
#'
#' @param expr Expression for the top-level task.
#' @param result Result of the top-level task.
#' @param successful A logical value indicating whether it was
#'        successfully completed or not (always \code{TRUE} at present).
#' @param printed A logical value indicating whether the result was
#'        printed or not.
#'
#' @return A logical value indicating whether to keep this function in
#'         the list of active callback.
update_current_commit <- function (expr, result, successful, printed)
{
  if (!state$tracking || !successful)
    return(TRUE)
  
  tryCatch({
      update_with_hash(globalenv())
      new_commit <- commit_from(globalenv(), state$last, expr)
      if (!commits_equal(new_commit, state$last)) {
        eapply(globalenv(), function(x)add_object(state$stash, x))
        add_object(state$stash, new_commit)
        state$last <- new_commit
      }
    },
    error = function(e) warning('could not create a commit: ', e$message, call. = FALSE)
  )
  
  TRUE
}


#' Add hash attribute to each object in environment.
#' 
#' @param env Environment to process.
update_with_hash <- function (env)
{
  lapply(ls(envir = env), function(name) {
    obj <- env[[name]]
    attr(env[[name]], hash_attribute_name) <- hash(obj)
  })
  invisible()
}


# --- actual commit-related code ---------------------------------------

is_commit <- function(x) inherits(x, 'commit')

empty_commit <- function()
  structure(
    list(objects = data.frame(), history = NA, hash = '', parent = NA),
    class = 'commit'
  )


#' Generates commit from an environment.
#' 
#' @param env Environment to process.
#' @return A commit object.
#' 
commit_from <- function (env, parent, history)
{
  stopifnot(is_commit(parent))
  
  nms <- sort(ls(envir = env))
  hsh <- lapply(nms, function(n) attr(env[[n]], hash_attribute_name,
                                      exact = TRUE))
  # make sure all objects have hash attribute assigned
  stopifnot(all(!vapply(hsh, is.null, logical(1))))
  
  obj <- data.frame(name = nms, hash = unlist(hsh), stringsAsFactors = FALSE)
  cmt <- structure(list(objects = obj, history = history, hash = '',
                        parent = parent$hash),
                   class = 'commit')
  cmt$hash <- hash(cmt)
  cmt
}


#' Compare two commit objects.
#' 
#' @param a First commit.
#' @param b Second commit.
#' @return \code{TRUE} if commits have the same lists of objects.
#' 
commits_equal <- function (a, b)
{
  stopifnot(is_commit(a) && is_commit(b))
  if (nrow(a$objects) != nrow(b$objects))
    return(FALSE)
  identical(a$objects[sort(a$objects$name), ], b$objects[sort(b$objects$name), ])
}

