context("track")


test_that("commit is updated", {
  state <- temp_state()
  env <- as.environment(list(x = 1))
  exp <- substitute(x <- 1)
  update_current_commit(state, env, NULL, exp)

  id <- storage::os_list(state$stash)
  expect_length(id, 2)
  expect_true(storage::compute_id(1) %in% id)
})


test_that("plot is cached", {

})

