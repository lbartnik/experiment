new_plot <- function ()
{
  on.exit(dev.off())
  png(tempfile(fileext = '.png'))
  plot(1:10)
  recordPlot()
}
