experimentOutput <- function(outputId, width = 'auto', height = 'auto') {
  htmlwidgets::shinyWidgetOutput(outputId, "experiment", width, height, package = "experiment")
}


renderExperiment <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, experimentOutput, env, quoted = TRUE)
}


gui_state <- as.environment(list(
  popup_clicked = FALSE
))


#' RStudio AddIn function.
#'
#' @description `browserAddin` runs the `htmlwidget` implemented in this
#' package as a RStudio AddIn (see [shiny::runGadget] for details).
#'
#' @param steps A `steps` data structure, see [fullhistory] for an example.
#'
#' @export
browserAddin <- function (steps = fullhistory())
{
  stopifnot(is_steps(steps))
  if (!count(steps)) {
    stop('history is empty, not showing the browser', call. = FALSE)
  }
  if (is.na(internal_state$task_callback_id)) {
    stop('tracking must be turned on in inder to open the widget',
         call. = FALSE)
  }

  # the definition of the UI
  ui <- shiny::shinyUI(miniUI::miniPage(
    miniUI::gadgetTitleBar(title = "Interactive Object Browser",
                           left  = miniUI::miniTitleBarCancelButton(),
                           right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)),
    miniUI::miniContentPanel(experimentOutput('experiment'),
                             padding = 15, scrollable = FALSE)
  ))

  welcomeMessage <- paste(
    'Choose a node (an object or a plot) on the graph. When the choice',
    'is made, click on the "Done" button and this will restore the state',
    'of R session when that object or plot was created.'
  )

  widget_opts <- list(welcome = welcomeMessage)
  if (isTRUE(gui_state$popup_clicked)) widget_opts$welcome <- NULL

  server <- function(input, output) {
    output$experiment <- renderExperiment(render_steps(steps, widget_opts))

    ## Your reactive logic goes here.

    # Listen for the 'done' event. This event will be fired when a user
    # is finished interacting with your application, and clicks the 'done'
    # button.
    #
    # Here is where your Shiny application might now go an affect the
    # contents of a document open in RStudio, using the `rstudioapi` package.
    shiny::observeEvent(input$done, {
      # we can safely assume that tracking is turned on, otherwise there
      # would be no history to look at
      if (is_empty(input$object_selected)) {
        cat('\nSelection empty, R session left unchanged.\n')
        hline()
      } else {
        st <- step_by(steps, input$object_selected)
        onRestore(st$commit_id)
      }

      # At the end, your application should call 'stopApp()' here, to ensure that
      # the gadget is closed after 'done' is clicked.
      shiny::stopApp()
    })

    shiny::observe({
      if (!is_empty(input$object_selected))
        onClick(steps, input$object_selected)
    })

    shiny::observe({
      if (isTRUE(input$popup_clicked))
        gui_state$popup_clicked <- TRUE
    })
  }

  onStart(welcomeMessage)
  tryCatch({
    suppressMessages({
      shiny::runGadget(ui, server, viewer = shiny::dialogViewer("Interactive Browser", width = 750))
    })
  }, error = function (e) {
    if (identical(e$message, 'User cancel'))
      onCancel()
    else
      stop(e$message, call. = TRUE)
  })
}


hline <- function ()  cat0(paste(rep_len('-', getOption('width')), collapse = ''), '\n\n')


onStart <- function (message)
{
  hline()
  cat(paste(strwrap(message, width = getOption('width')), collapse = '\n'))
  cat('\n\n')
}


onClick <- function (steps, id)
{
  stopifnot(!is_empty(id))

  st <- step_by(steps, id = id)
  co <- commit_restore(st$commit_id, internal_state$stash, .data = FALSE)

  cat('\n')
  cat0(crayon::green ('Chosen'), ': ', st$type, ' `', st$name, '` (id: ', st$id, ')\n')
  cat0(crayon::yellow('belongs to commit'), ': ', co$id, '\n')
  cat0('\n')

  print(co, header = FALSE)
}


onRestore <- function (commit_id)
{
  cat0('\n\n', crayon::green('Restoring commit'), ': ', commit_id, '\n')
  hline()
  restore_commit(internal_state, commit_id, globalenv())
}


onCancel <- function ()
{
  cat('\nUser cancel, commit will not be restored.\n')
  hline()
}



# temporary utility function
attachStore <- function (path = file.path(getwd(), "project-store"))
{
  store <- prepare_object_store(path)
  reattach_to_store(internal_state, store, globalenv(), "overwrite")
  invisible()
}

