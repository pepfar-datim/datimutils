test_that("can log in to play", {
  testthat::expect_true(DHISLogin_Play())
  testthat::expect_error(DHISLogin_Play(3.0))
})
