showDialog <- function (title, ...) {
  rstudioapi::showDialog(title, paste(...), url = '')
}

showChoiceDialog <- function (title, message, choices) {
  stopifnot(all_named(choices))

  mapply(function(name, desc) {
    cat(crayon::green(name), ': ', desc, '\n')
  }, name = names(choices), desc = as.character(choices))

  message <- paste0(message, '\n\n',
                    paste0(seq_along(choices), ': ', names(choices), collapse = '\n'),
                    paste0(rep('\n', length(choices)), collapse = ''))

  repeat {
    Sys.sleep(.1) # gives RStudio time to scroll down the console
    ans <- rstudioapi::showPrompt(title, message, default = "1-based index or value")

    if (is.null(ans)) return(NULL)

    # check if it is a value
    if (!is.na(i <- match(ans, names(choices)))) {
      ans <- i
    }

    # check if can be cast as integer and whether points to a choice
    ans <- tryCatch(as.integer(ans), warning = function (e) NULL)
    if (!is.null(ans) && between(ans, 1, length(choices))) break

    # let the user know something went wrong
    showDialog("Error", "Could not recognize that choice...")
  }

  nth(names(choices), ans)
}
