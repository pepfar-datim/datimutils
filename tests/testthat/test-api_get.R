context("make arbitrary api call DATIM")

library(httptest)

#code to create/update mocks

# httptest::start_capturing(simplify = FALSE)
# #not logged in for this one, fields there just to give the call a unique
# httptest ID
# httr::GET(
# "https://play.dhis2.org/2.33/api/me.json?paging=false&fields=notloggedin")
# httptest::stop_capturing()
#
# datapackcommons::DHISLogin_Play("2.33")
#
# httptest::start_capturing(simplify = FALSE)
#
# httr::GET("https://play.dhis2.org/2.33/apii/me.json?paging=false")
# httr::GET("https://play.dhis2.org/2.33/api/me.json?paging=false")
# httr::GET("https://play.dhis2.org/2.33/api/me.json?paging=false&fields=name")
# httr::GET(
# "https://play.dhis2.org/2.33/api/indicators.json?paging=false&fields=name")
# httr::GET(
# "https://play.dhis2.org/2.33/api/indicators/ReUHfIn0pTQ.json?paging=false")
# httr::GET(
#   paste0(
#     "https://play.dhis2.org/2.33/api/indicators.json?paging=false",
#     "&fields=name,id,translations[locale,value],indicatorGroups[id,name]
#      &filter=name:ilike:anc"
#   )
# )
# httr::GET("https://play.dhis2.org/2.33/api/30/smsCommands.json?paging=false")
#
# httptest::stop_capturing()


################################################################################
# TEST 1
################################################################################
# no mock for this test
testthat::test_that("Can use timeout paramater", {
  skip_if_disconnected()
  # timeout should be short enough to trip this error but
  # an internet connection is required
  expect_error(api_get(
    path = "organisationUnits?timeout",
    d2_session = play40.0.1,
    timeout = .001
  ))
})

################################################################################
# TEST 2
################################################################################
# no mock for this test - no httr requests should actually be issued
# for these response formats
# we expect only json requests
testthat::test_that("We reject request for non-json response formats", {
  formats <- c(
    ".jsonp", ".html", ".xml", ".pdf",
    ".xls", ".csv", ".html+css", ".adx"
  )
  helper <- function(x) {
    # we should get an error for unsupported response format in URL
    expected_error <- expect_error(
      api_get(
        path = paste0(
          "api/analytics", x,
          "?dimension=dx:fbfJHSPpUQD",
          "&dimension=pe:LAST_12_MONTHS",
          "&filter=ou:ImspTQPwCqd"
        ),
        d2_session = play40.0.1
      )
    )
    # when run without_internet the error message should start with GET
    # see details of httptest::with_internet
    expect_false(grepl("GET ", expected_error$message))
  }
  without_internet(lapply(formats, helper))
})
################################################################################

# API Testing begins using httptest package
httptest::with_mock_api({

  # TEST 3
  ##############################################################################
  testthat::test_that("We handle anticipated api issues", {
    # non-json content type
    # mock built when not logged in resulting in content type of
    # html from the login page
    testthat::expect_error(
      # httr::GET("https://play.dhis2.org/40.0.1/api/me.json?paging=
      # false&fields=notloggedin")
      api_get(
        path = "api/me?fields=notloggedin",
        d2_session = play40.0.1,
        retry = 1, timeout = 60, api_version = NULL
      )
    )
    # no base_url
    testthat::expect_error(
      api_get(path = "api/me2",
              d2_session = NULL))
    # response status !=200
    testthat::expect_error(
      # httr::GET("https://play.dhis2.org/2.33/apii/me.json?paging=false")
      api_get(
        path = "apii/me",
        d2_session = play40.0.1,
        retry = 1,
        timeout = 60,
        api_version = NULL
      ))
      testthat::expect_error(
        # httr::GET("http://httpstat.us/504")
        api_get(path = "504", retry = 2,
                d2_session = list(base_url = "http://httpstat.us/")))
  })

  ##############################################################################
  # TEST 4
  ##############################################################################
  testthat::test_that(
    "basic calls: https://play.dhis2.org/40.0.1/api/me.json?paging=false", {
      user <- api_get(
        path = "api/me",
        d2_session = play40.0.1
      )
      testthat::expect_identical(user$name, "John Traore")
      testthat::expect_identical(user$id, "xE7jOejl9FI")
      rm(user)

      user <- api_get(
        path = "api/me.json",
        d2_session = play40.0.1
      )
      testthat::expect_identical(user$name, "John Traore")
      rm(user)

      user <- api_get(
        path = "api/me.json?paging=false",
        d2_session = play40.0.1
      )
      testthat::expect_identical(user$name, "John Traore")
      rm(user)

      user <- api_get(
        path = "api/me/",
        d2_session = play40.0.1
      )
      testthat::expect_identical(user$name, "John Traore")
      rm(user)
    }
  )

  ##############################################################################
  # TEST 5
  ##############################################################################
  testthat::test_that(
    paste0(
      "Specific id: https://play.dhis2.org/2.33/",
      "api/indicators/ReUHfIn0pTQ.json?paging=false"
    ), {
      ind <- api_get(
        path = "api/indicators/ReUHfIn0pTQ",
        d2_session = play40.0.1
      )
      testthat::expect_identical(ind$name, "ANC 1-3 Dropout Rate")
      rm(ind)
    }
  )

  ##############################################################################
  # TEST 6
  ##############################################################################
  testthat::test_that(paste0(
    "Nested fields: ",
    "https://play.dhis2.org/2.33/api/indicators.json?paging=false",
    "&fields=name,id,translations[locale,value],indicatorGroups[id,name]",
    "&filter=name:ilike:anc"
  ), {
    ind <- api_get(
      path = paste0(
        "api/indicators&fields=name,id,translations[locale,value],",
        "indicatorGroups[id,name]&filter=name:ilike:anc"
      ),
      d2_session = play40.0.1
    )
    expect_type(ind[["indicators"]][["translations"]][[1]], "list")
    expect_named(ind[["indicators"]][["indicatorGroups"]][[1]], c("name", "id"))
    expect_true(all(grepl("[Aa][Nn][Cc]", ind$indicators$name)))
    rm(ind)
  })

  ##############################################################################
  # TEST 7
  ##############################################################################
  testthat::test_that(paste0(
    "Specific field: https://play.dhis2.org/2.33/",
    "api/me.json?paging=false&fields=name"
  ), {
    user <- api_get(
      path = "api/me?fields=name",
      d2_session = play40.0.1
    )
    testthat::expect_identical(user$name, "John Traore")
    testthat::expect_null(user$id)
    rm(user)
  })

  ##############################################################################
  # TEST 8
  ##############################################################################
  testthat::test_that(paste0(
    "Pagination: https://play.dhis2.org/2.33/",
    "api/indicators.json?paging=false&fields=name"
  ), {
    # standard call to indicators would have pagination
    # check we are not receiving paged results if we leave off paging=false
    ind <- api_get(
      path = "api/indicators?fields=name",
      d2_session = play40.0.1
    )
    testthat::expect_null(ind$pager)
    rm(ind)
  })

  ##############################################################################
  # TEST 9
  ##############################################################################
  testthat::test_that("Can use API versioning", {
    data <- api_get(
      path = "smsCommands",
      d2_session = play40.0.1,
      api_version = "30"
    )
    expect_gt(length(data), 0)
    rm(data)
  })
})
