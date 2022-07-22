# code to create/update mocks
#  library(httptest)
# #
#  httptest::start_capturing(simplify = FALSE)
#  httr::content(
#    httr::GET("https://play.dhis2.org/2.36.3/api/dataStore/metabase/repositories",
#            httr::authenticate("admin", "district"))
#  )
#
#  httr::content(
#    httr::GET("https://play.dhis2.org/2.36.3/api/dataStore/metabase/foo",
#              httr::authenticate("admin", "district"))
#  )
#
# httptest::stop_capturing()

context("Get a datastore key")
with_mock_api({

  test_that("Get a datstore object ", {

    data <- getDataStoreKey("metabase", "repositories", d2_session = play2363)

    testthat::expect_type(data, "list")
    testthat::expect_equal(length(data), 2)
    rm(data) })

  test_that("Can error with missing namespace", {
    expect_error(getDataStoreKey(NA, "repositories", d2_session = play2363))
  })

  test_that("Can error with missing key", {
    expect_error(getDataStoreKey("repositories", NA, d2_session = play2363))
  })

  test_that("Can error with a list as namespace", {
    expect_warning(data <- getDataStoreKey("metabase", "foo", d2_session = play2363),
                   "The requested datastore object could not be found")
    expect_null(data)
  })



})
