experimentOutput <- function(outputId, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(outputId, "experiment", width, height, package = "experiment")
}


renderExperiment <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, experimentOutput, env, quoted = TRUE)
}


# temporary utility function
attachStore <- function (path = file.path(getwd(), "project-store"))
{
  store <- prepare_object_store(path)
  reattach_to_store(internal_state, store, globalenv(), "overwrite")
  invisible()
}



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

  ui <- shiny::shinyUI(miniUI::miniPage(
    miniUI::gadgetTitleBar(title = "Interactive Object Browser",
                           left  = miniUI::miniTitleBarCancelButton(),
                           right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)),
    miniUI::miniContentPanel(experimentOutput('experiment'),
                             padding = 15, scrollable = TRUE)
  ))


  server <- function(input, output) {
    output$experiment <- renderExperiment(plot(steps))

    ## Your reactive logic goes here.

    # Listen for the 'done' event. This event will be fired when a user
    # is finished interacting with your application, and clicks the 'done'
    # button.
    shiny::observeEvent(input$done, {

      # Here is where your Shiny application might now go an affect the
      # contents of a document open in RStudio, using the `rstudioapi` package.
      #
      # At the end, your application should call 'stopApp()' here, to ensure that
      # the gadget is closed after 'done' is clicked.
      shiny::stopApp()
    })

    shiny::observe({
      if (!is.null(input$object_selected))
        onClick(steps, input$object_selected)
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("Interactive Browser", width = 750))
}


onClick <- function (steps, object_id)
{
  st <- step_by_id(steps, object_id)
  co <- commit_restore(st$commit_id, internal_state$stash, .data = TRUE)
  print(co)
}

