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
runShiny <- function (data)
{
  stopifnot(is_steps(data))

  ui = shinyUI(fluidPage(
    experimentOutput('experiment')
  ))

  server = function(input, output) {
    output$experiment <- renderExperiment(plot(data))
  }

#  shinyApp(ui = ui, server = server)
  shiny::runGadget(ui, server)
}


# temporary utility function
attachStore <- function (path = file.path(getwd(), "project-store"))
{
  store <- prepare_object_store(path)
  reattach_to_store(internal_state, store, globalenv(), "abort")
  invisible()
}
