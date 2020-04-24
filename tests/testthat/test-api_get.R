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
  httr::GET("https://play.dhis2.org/2.33/api/indicators/ReUHfIn0pTQ.json?paging=false")
  httr::GET(paste0("https://play.dhis2.org/2.33/api/indicators.json?paging=false",
                   "&fields=name,id,translations[locale,value],indicatorGroups[id,name]&filter=name:ilike:anc")
            )
  httr::GET("https://play.dhis2.org/2.33/api/30/smsCommands.json?paging=false")
  httptest::stop_capturing()
  }

# no mock for this test
test_that("Can use timeout paramater", {
  skip_if_disconnected()
  # timeout should be short enough to trip this error but 
  # an internet connection is required
  expect_error(api_get(path = "organisationUnits?timeout",
                       base_url =  "https://play.dhis2.org/2.33/",
                       timeout = .001)
  )
})

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
 
 test_that("basic calls: https://play.dhis2.org/2.33/api/me.json?paging=false", {
   
   user <- api_get(path = "api/me",
                   base_url =  "https://play.dhis2.org/2.33/")
   testthat::expect_identical(user$name, "John Traore")
   testthat::expect_identical(user$id, "xE7jOejl9FI")
   rm(user)
   
   user <- api_get(path = "api/me.json",
                   base_url =  "https://play.dhis2.org/2.33/")
   testthat::expect_identical(user$name, "John Traore")
   rm(user)
   
   user <- api_get(path = "api/me.json?paging=false",
                   base_url =  "https://play.dhis2.org/2.33/")
   testthat::expect_identical(user$name, "John Traore")
   rm(user)
   
   user <- api_get(path = "api/me.csv",
                   base_url =  "https://play.dhis2.org/2.33/")
   testthat::expect_identical(user$name, "John Traore")
   rm(user)
   
   user <- api_get(path = "api/me.csv?paging=false",
                   base_url =  "https://play.dhis2.org/2.33/")
   testthat::expect_identical(user$name, "John Traore")
   rm(user)
   
   user <- api_get(path = "api/me/",
                   base_url =  "https://play.dhis2.org/2.33/")
   testthat::expect_identical(user$name, "John Traore")
   rm(user)
 })
 
 test_that("Specific id: https://play.dhis2.org/2.33/api/indicators/ReUHfIn0pTQ.json?paging=false", {  
   
   ind <- api_get(path = "api/indicators/ReUHfIn0pTQ",
                   base_url =  "https://play.dhis2.org/2.33/")
   testthat::expect_identical(ind$name, "ANC 1-3 Dropout Rate")
   rm(ind)
 })
 
 test_that(paste0("Nested fields: ",
                  "https://play.dhis2.org/2.33/api/indicators.json?paging=false",
                  "&fields=name,id,translations[locale,value],indicatorGroups[id,name]&filter=name:ilike:anc"), {  
   
   ind <- api_get(path = 
                    paste0("api/indicators",
                           "&fields=name,id,translations[locale,value],indicatorGroups[id,name]&filter=name:ilike:anc"),
                  base_url =  "https://play.dhis2.org/2.33/")
   expect_type(ind[["indicators"]][["translations"]][[1]], "list")
   expect_named(ind[["indicators"]][["indicatorGroups"]][[1]], c("name", "id"))
   expect_true(all(grepl("[Aa][Nn][Cc]", ind$indicators$name)))
   rm(ind)
 })
 
 test_that("Specific field: https://play.dhis2.org/2.33/api/me.json?paging=false&fields=name", {  
   
   user <- api_get(path = "api/me?fields=name",
                   base_url =  "https://play.dhis2.org/2.33/")
   testthat::expect_identical(user$name, "John Traore")
   testthat::expect_null(user$id)
   rm(user)
 })
 
 test_that("Pagination: https://play.dhis2.org/2.33/api/indicators.json?paging=false&fields=name", {
# standard call to indicators would have pagination
# check we are not receiving paged results if we leave off paging=false
   ind <- api_get(path = "api/indicators?fields=name",
                  base_url =  "https://play.dhis2.org/2.33/")
   testthat::expect_null(ind$pager)
   rm(ind)
 })
 
 test_that("Can use API versioning", {
   data <- api_get(path = "smsCommands",
           base_url =  "https://play.dhis2.org/2.33/",
           api_version = "30")
   expect_gt(length(data), 0)
   rm(data)
   })
 })