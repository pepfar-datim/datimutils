context("make arbitrary api call DATIM")

# Version of DHIS 2 assumed in these unit tests and used in mocks
d2_version = "2.33"
base_url <- glue::glue("https://play.dhis2.org/{d2_version}/")

# httptest::start_capturing()
# #not logged in for this one
# api_get(path = "api/me?fields=notloggedin", base_url = base_url,
#         retry = 1, timeout = 60,
#         api_version = NULL)
# httptest::stop_capturing()


#
# api_get(path = "api/me", base_url = base_url,
#         retry = 1, timeout = 60,
#         api_version = NULL)
# api_get(path = "api/me.json", base_url = base_url,
#         retry = 1, timeout = 60,
#         api_version = NULL)
# api_get(path = "api/me.xml", base_url = base_url,
#         retry = 1, timeout = 60,
#         api_version = NULL)
#httptest::stop_capturing()

httptest::with_mock_api({
test_that("We get expected errors", {
  #  Not logged in yet
  testthat::expect_error(
    api_get(path = "api/me?fields=notloggedin", base_url = base_url,
            retry = 1, timeout = 60,
            api_version = NULL))
  
  
})
  

#   test_that("We can get a user object", {
#   
# #  Not logged in yet
#     testthat::expect_error(
#       api_get(path = "api/me?fields=notloggedin", base_url = base_url,
#               retry = 1, timeout = 60,
#               api_version = NULL))
#   
#     datapackcommons::DHISLogin_Play("2.33")
#     
    user <- api_get(path = "api/me", base_url = base_url,
                    retry = 1, timeout = 60,
                    api_version = NULL)
#     testthat::expect_identical(user$name, "John Traore")
#     user <- api_get(path = "api/me", base_url = base_url,
#                     retry = 1, timeout = 60,
#                     api_version = NULL)
#     testthat::expect_identical(user$name, "John Traore")
#     user <- api_get(path = "api/me.json", base_url = base_url,
#                     retry = 1, timeout = 60,
#                     api_version = NULL)
#     testthat::expect_identical(user$name, "John Traore")
#     user <- api_get(path = "api/me?fields=name", base_url = base_url,
#                     retry = 1, timeout = 60,
#                     api_version = NULL)
#     testthat::expect_identical(user$name, "John Traore")
#     testthat::expect_error(
#       api_get(path = "apii/", base_url = base_url,
#               retry = 1, timeout = 60,
#               api_version = NULL
#       )
#     )
#     testthat::expect_error(
#       api_get(path = "dhis-web-commons-about/about.action", base_url = base_url,
#               retry = 1, timeout = 60,
#               api_version = NULL
#       )
#     )
#   })
 })
