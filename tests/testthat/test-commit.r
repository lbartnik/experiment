context("commit")

test_that("hash values are assigned inside environment", {
  e <- environment()
  e$a <- 1
  e$b <- list(x = 2)
  e$c <- data.frame(d = 3, e = 5)
  
  update_with_hash(e)
  eapply(e, function(x) expect_true(hash_attribute_name %in% names(attributes(x))))
})
