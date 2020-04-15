context("make arbitrary api call DATIM")

with_mock_api({
  test_that("We can get a user object", {
    user <- api_get(path = "api/me", base_url = "www.datim.org", retry = 1, timeout = 60,
                                api_version = NULL ) 
    expect_identical(user$name, "FakeName")
  })
})


  