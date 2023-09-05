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

    data <- getDataStoreKey("dedupe", "periodSettings", d2_session = test)

    expect_named(data, c("RESULTS", "TARGETS"))

    expect_type(data, "list")
    expect_equal(length(data), 2)
     })

  test_that("Can error with missing namespace", {
    expect_error(getDataStoreKey(NA, "bar", d2_session = test))
  })

  test_that("Can error with missing key", {
    expect_error(getDataStoreKey("foo", NA, d2_session = test))
  })

  test_that("Can error with a list as namespace", {
    expect_warning(data <- getDataStoreKey("dataQualityTool", "foo", d2_session = test),
                   "api/dataStore/dataQualityTool/foo could not be retreived from the server")
    expect_null(data)

  test_that("Can get a list of namespace keys", {
    mykeys <- getDataStoreNamespaceKeys("dedupe",  d2_session = test)
    expect_setequal(mykeys, c("crosswalks", "periodSettings", "report", "reportSettings"))
  })

  test_that("Can get a list of namespaces from the datastore", {
            mynamespaces <- getDataStoreNamespaces(d2_session = test)
            expect_true(inherits(mynamespaces, "list"))
            })

  })

  test_that("Get a datstore object ", {

    data <- getDataStoreKey("dataQualityTool", "settings", d2_session = play40.0.1)

    testthat::expect_type(data, "list")
    testthat::expect_equal(length(data), 9)
    rm(data) })

  test_that("Can error with missing namespace", {
    expect_error(getDataStoreKey(NA, "settings", d2_session = play40.0.1))
  })

  test_that("Can error with missing key", {
    expect_error(getDataStoreKey("dataQualityTool", NA, d2_session = play40.0.1))
  })


})
