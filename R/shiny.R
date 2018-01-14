#' @export
#' @importFrom htmlwidgets shinyWidgetOutput
experimentOutput <- function(outputId, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(outputId, "experiment", width, height, package = "experiment")
}

#' @export
#' @importFrom htmlwidgets shinyRenderWidget
renderExperiment <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, experimentOutput, env, quoted = TRUE)
}


#' @importFrom shiny shinyUI fluidPage checkboxInput shinyApp
#' @export
browserAddin <- function (data = fullhistory())
{
  stopifnot(is_steps(data))

  ui = shinyUI(miniUI::miniPage(
    miniUI::gadgetTitleBar(title = "Interactive Object Browser",
                           left = miniTitleBarCancelButton(),
                           right = miniTitleBarButton("done", "Done", primary = TRUE)),
    miniUI::miniContentPanel(experimentOutput('experiment'),
                             padding = 15, scrollable = TRUE)
  ))

  server = function(input, output) {
    output$experiment <- renderExperiment(plot(data))

    ## Your reactive logic goes here.

    # Listen for the 'done' event. This event will be fired when a user
    # is finished interacting with your application, and clicks the 'done'
    # button.
    observeEvent(input$done, {

      # Here is where your Shiny application might now go an affect the
      # contents of a document open in RStudio, using the `rstudioapi` package.
      #
      # At the end, your application should call 'stopApp()' here, to ensure that
      # the gadget is closed after 'done' is clicked.
      stopApp()
    })
  }

#  shinyApp(ui = ui, server = server)
  shiny::runGadget(ui, server, viewer = dialogViewer("Interactive Browser", width = 750))
}


# temporary utility function
attachStore <- function (path = file.path(getwd(), "project-store"))
{
  store <- prepare_object_store(path)
  reattach_to_store(internal_state, store, globalenv(), "overwrite")
  invisible()
}
