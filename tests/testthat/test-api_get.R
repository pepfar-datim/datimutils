context("make arbitrary api call DATIM")

code_used_to_generate_mock_requests <- function() {
  
  library(httptest)
  
  httptest::start_capturing(simplify = FALSE)
  #not logged in for this one, fields there just to give the call a unique httptest ID
  httr::GET("https://play.dhis2.org/2.33/api/me.json?paging=false&fields=notloggedin")
  httptest::stop_capturing()
  
  datapackcommons::DHISLogin_Play("2.33")

  httptest::start_capturing(simplify = FALSE)
  httr::GET("https://play.dhis2.org/2.33/apii/me.json?paging=false")
  httr::GET("https://play.dhis2.org/2.33/api/me.json?paging=false")
  httr::GET("https://play.dhis2.org/2.33/api/me.json?paging=false&fields=name")
  httr::GET("https://play.dhis2.org/2.33/api/indicators.json?paging=false&fields=name")
  httptest::stop_capturing()
  }

httptest::with_mock_api({
  test_that("We handle anticipated api issues", {
# non-json content type
# mock built when not logged in resulting in content type of
# html from the login page
    testthat::expect_error(
# https://play.dhis2.org/2.33/api/me.json?paging=false&fields=notloggedin
      api_get(path = "api/me?fields=notloggedin", 
              base_url = "https://play.dhis2.org/2.33/",
              retry = 1, timeout = 60, api_version = NULL
              )
      )
    
# response status !=200   
    testthat::expect_error(
# https://play.dhis2.org/2.33/apii/me.json?paging=false
      api_get(path = "apii/me",
              base_url = "https://play.dhis2.org/2.33/",
              retry = 1, timeout = 60,
              api_version = NULL
      )
    )
  })
 
 
 
 test_that("We can issue basic api calls", {

# "https://play.dhis2.org/2.33/api/me.json?paging=false"
   user <- api_get(path = "api/me",
                   base_url =  "https://play.dhis2.org/2.33/")
   testthat::expect_identical(user$name, "John Traore")
   
   user <- api_get(path = "api/me/",
                   base_url =  "https://play.dhis2.org/2.33/")
   testthat::expect_identical(user$name, "John Traore")
   
   user <- api_get(path = "api/me.json",
                   base_url =  "https://play.dhis2.org/2.33/")
   testthat::expect_identical(user$name, "John Traore")
   
   user <- api_get(path = "api/me.csv",
                   base_url =  "https://play.dhis2.org/2.33/")
   testthat::expect_identical(user$name, "John Traore")
   
   user <- api_get(path = "api/me.csv?paging=false",
                   base_url =  "https://play.dhis2.org/2.33/")
   testthat::expect_identical(user$name, "John Traore")
   
# https://play.dhis2.org/2.33/api/me.json?paging=false&fields=name   
   user <- api_get(path = "api/me?fields=name",
                   base_url =  "https://play.dhis2.org/2.33/")
   testthat::expect_identical(user$name, "John Traore")
   testthat::expect_null(user$id)

# standard call to indicators would have pagination
# check we are not receiving paged results if we leave off paging=false
# https://play.dhis2.org/2.33/api/indicators.json?paging=false&fields=name   
   ind <- api_get(path = "api/indicator?fields=name",
                   base_url =  "https://play.dhis2.org/2.33/")
   testthat::expect_null(ind$pager)
   })
 })
 
 test_that("Can use extra parameters", {
   skip_if_disconnected()
   #timeout should be short enough to trip this but an internet connection is required
   expect_error(api_get(path = "organisationUnits?timeout",
                        base_url =  "https://play.dhis2.org/2.33/",
                        timeout = .001)
                )
 })