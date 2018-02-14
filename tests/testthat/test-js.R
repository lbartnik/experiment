context("JS")

test_that("JS tests pass", {
  skip("fix: browser does not exit")

  expect_true(unittestGadget(sample_steps(), browser = TRUE, port = shinyPort))
})



# This could work if webdriver had asynchronous operations. At this point
# ses$go() is synchronous and the code deadlocks.
#
#
# webdriver::install_phantomjs()
# pjs <- webdriver::run_phantomjs()
# ses <- webdriver::Session$new(port = pjs$port)
#
# shinyPort <- as.integer(1024 + runif(1) * 48000)
#
# opt <- options(browser = function (url) {
#   cat('going to ', url, '\n')
#   ses$executeScriptAsync(paste0('window.location="', url, '";'))
# })
# on.exit(options(opt), add = TRUE)
#
# expect_true(unittestGadget(browser = TRUE, port = shinyPort))
#
# cat(ses$getUrl(), '\n')
# ses$takeScreenshot(file = '~/shot.png')
#
