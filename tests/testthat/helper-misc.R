dummy_plot <- function ()
{
  on.exit(dev.off())
  png(tempfile(fileext = '.png'))
  dev.control("enable")
  plot(seq(10))
  recordPlot()
}

random_plot <- function ()
{
  on.exit(dev.off())
  png(tempfile(fileext = '.png'))
  dev.control("enable")
  plot(rnorm(10))
  recordPlot()
}
