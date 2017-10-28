context("track")

test_that("commit is updated", {
  env <- as.environment(list(x = 1))
  exp <- substitute(x <- 1)
  experiment:::update_current_commit(env, exp)
  
  id <- storage::os_list(experiment:::stash())
  expect_length(id, 2)
  expect_true(storage::compute_id(1) %in% id)
})
