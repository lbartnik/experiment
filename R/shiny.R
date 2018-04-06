experimentOutput <- function(outputId, width = '100%', height = '100%') {
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
#' @import miniUI
#' @importFrom shiny dialogViewer observeEvent runGadget shinyUI stopApp
browserAddin <- function (steps = fullhistory())
{
  # error checking
  stopifnot(is_steps(steps))
  if (!count(steps)) {
    stop('history is empty, not showing the browser', call. = FALSE)
  }
  if (is.na(internal_state$task_callback_id)) {
    stop('tracking must be turned on in inder to open the widget',
         call. = FALSE)
  }

  # instructions on what to do with this widget
  if (!isTRUE(gui_state$popup_clicked)) {
    showDialog("Information",
       'In the Interactive History browser, choose a node (an object or a plot)',
       'on the graph. When the choice is made, click on the "Done" button and',
       'this will restore the state of R session when that object or plot was created.'
    )
    gui_state$popup_clicked <- TRUE
  }

  # --- the actual widget ---

  ui <- shinyUI(miniPage(
    gadgetTitleBar(title = "Interactive Object Browser",
                   left  = miniTitleBarCancelButton(),
                   right = miniTitleBarButton("done", "Done", primary = TRUE)),
    miniContentPanel(experimentOutput('experiment'),
                     padding = 15, scrollable = FALSE)
  ))

  server <- function(input, output) {
    output$experiment <- renderExperiment(render_steps(steps))

    # we can safely assume that tracking is turned on, otherwise there
    # would be no history to look at
    observeEvent(input$done, {
      if (is_empty(input$object_selected)) {
        cat('\nSelection empty, R session left unchanged.\n')
        hline()
      } else {
        st <- step_by(steps, input$object_selected)
        onRestore(st$commit_id)
      }

      stopApp(invisible(TRUE))
    })

    observeEvent(input$cancel, {
      cat('\nUser cancel, commit will not be restored.\n')
      hline()
      stopApp(invisible(FALSE))
    })

    observe({
      if (!is_empty(input$object_selected))
        onClick(steps, input$object_selected)
    })

    observe({
      if (!is_empty(input$comment) && !is_empty(input$object_selected))
        onCommentChange(steps, input$object_selected, input$comment)
    })
  }

  suppressMessages({
    runGadget(ui, server, viewer = dialogViewer("Interactive Browser", width = 750),
              stopOnCancel = FALSE)
  })
}


hline <- function ()  cat0(paste(rep_len('-', getOption('width')), collapse = ''), '\n\n')

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


onCommentChange <- function (steps, id, comment)
{
  st <- step_by(steps, id = id)
  tags <- storage::os_read_tags(internal_state$stash, st$object_id)
  tags$comment <- comment
  storage::os_update_tags(internal_state$stash, st$object_id, tags)
}


onRestore <- function (commit_id)
{
  cat0('\n\n', crayon::green('Restoring commit'), ': ', commit_id, '\n')
  hline()
  restore_commit(internal_state, commit_id, globalenv())
}





# temporary utility function
attachStore <- function (path = file.path(getwd(), "project-store"))
{
  store <- prepare_object_store(path)
  reattach_to_store(internal_state, store, globalenv(), "overwrite")
  invisible()
}


# --- basic history viewer ---------------------------------------------

historyOutput <- function(outputId, width = '100%', height = '100%') {
  htmlwidgets::shinyWidgetOutput(outputId, "browse", width, height, package = "experiment")
}

renderHistory <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) }
  htmlwidgets::shinyRenderWidget(expr, historyOutput, env, quoted = TRUE)
}


#' @import miniUI
#' @importFrom shiny browserViewer dialogViewer observeEvent runGadget shinyUI stopApp textOutput
historyGadget <- function (data = list())
{
  ui <- shinyUI(miniPage(
    gadgetTitleBar(title = "Basic History Browser",
                   left  = miniTitleBarCancelButton(),
                   right = miniTitleBarButton("done", "Done", primary = TRUE)),
    miniContentPanel(historyOutput('viewer'),
                     textOutput('closeWindow'),
                     padding = 15, scrollable = TRUE)
  ))

  server <- function(input, output) {
    output$viewer <- renderHistory(htmlwidgets::createWidget("browse", list(data = data)))

    observeEvent(input$done, { stopApp(TRUE) })
    observeEvent(input$cancel, { stopApp(FALSE) })
  }

  viewer <- dialogViewer("Interactive Browser")
  runGadget(ui, server, viewer = viewer, stopOnCancel = FALSE)
}



# --- unit tests in browser --------------------------------------------

unittestOutput <- function(outputId, width = '100%', height = '100%') {
  htmlwidgets::shinyWidgetOutput(outputId, "unittest", width, height, package = "experiment")
}

renderUnittest <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) }
  htmlwidgets::shinyRenderWidget(expr, unittestOutput, env, quoted = TRUE)
}


#' @import miniUI
#' @importFrom shiny browserViewer dialogViewer observeEvent runGadget shinyUI stopApp textOutput
unittestGadget <- function (data = system.file("htmlwidgets/data-1/data.json", package = 'experiment'),
                            browser = FALSE, autoClose = TRUE, port = NULL)
{
  if (is.character(data)) {
    data <- jsonlite::fromJSON(data, simplifyVector = FALSE)
  }

  ui <- shinyUI(miniPage(
    gadgetTitleBar(title = "Interactive Object Browser",
                   left  = miniTitleBarCancelButton(),
                   right = miniTitleBarButton("done", "Done", primary = TRUE)),
    miniContentPanel(unittestOutput('unittest'),
                     textOutput('closeWindow'),
                     padding = 15, scrollable = TRUE)
  ))

  server <- function(input, output) {
    stopApp <- function (rc) {
      if (!isTRUE(autoClose)) return()
      output$closeWindow <- renderText('done')
      shiny::stopApp(rc)
    }

    output$unittest <- renderUnittest(htmlwidgets::createWidget("unittest", list(data = data)))

    observeEvent(input$done, { stopApp(TRUE) })
    observeEvent(input$cancel, { stopApp(FALSE) })
  }

  viewer <- if (isTRUE(browser)) browserViewer() else dialogViewer("Interactive Browser")
  runGadget(ui, server, viewer = viewer, stopOnCancel = FALSE, port = port)
}
