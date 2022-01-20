library(httptest)
library(datimutils)

#.mockPaths("/var/somewhere/else")
# options(httptest.verbose=TRUE)

#capture_requests({
  
# Unsuccessful based on credentials.
  # loginToDATIM(
  #   base_url = 'play.dhis2.org/2.372/',
  #   username = 'admin',
  #   password = 'test')
# Succesful 
#  loginToDATIM(
#     base_url = 'play.dhis2.org/2.37/',
#     username = 'admin',
#     password = 'district')
  
# Unsuccessful based on bad base_url.
   # loginToDATIM(
   #    base_url = 'play.dhis2.org/badBaseUrl/',
   #    username = 'admin',
   #    password = 'district')

#})

################################################################################
# TEST 1 
################################################################################
context("Get an unsuccessful login based upon credentials")

httptest::with_mock_api({
  
  test_that("We cannot get logged in successfully due to credentials", {
    
    testthat::expect_error(loginToDATIM(
      #Found in folder 2.372/api/
      base_url = 'play.dhis2.org/2.372/',
      username = 'admin',
      password = 'test'
    ),
      "Unable to authenticate due to an invalid username or password. 
         Please update your credentials and try again.",
    fixed=TRUE #Checks against a regular expression so must be here
    )
  })
})
################################################################################
# TEST 2 
################################################################################
context("Get a successful login")

httptest::with_mock_api({
  
  test_that("We get logged in successfully", {
    
    x=loginToDATIM(
      #Found in folder 2.37/api/me.json
      base_url = 'play.dhis2.org/2.37/',
      username = 'admin',
      password = 'district'
    )
    
    testthat::expect_identical(x$me$userCredentials$username, "admin")

  })
})
################################################################################
# TEST 3 
################################################################################
context("Get an unsuccessful login based upon base_url")

httptest::with_mock_api({
  
  test_that("We cannot get logged in successfully due to an invalid base_url", {
    
    testthat::expect_error(loginToDATIM(
      #Found in folder 2.372/api/
      base_url = 'play.dhis2.org/badBaseUrl/',
      username = 'admin',
      password = 'test'
    ),
    "Unable to authenticate due to an invalid URL.Please check the
         'base_url' parameter you provided.",
    fixed=TRUE #Checks against a regular expression so must be here
    )
  })
})
