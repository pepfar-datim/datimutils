context("Login to DATIM")

with_mock_api({
  test_that("We can get a user object", {
    user <- loginToDATIM(ring  = "DatimLogin")
    #user <- httr::GET("www.datim.org/api/me")
    #expect_identical(user$name, "FakeName")
  })
})


