library(httptest)
library(datimutils)

#capture_requests({

# Unsuccessful based on credentials.
  # loginToDATIM(
  #   base_url = 'play.dhis2.org/2.372/',
  #   username = 'admin',
  #   password = 'test')
# Succesful
# loginToDATIM(
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
      base_url = "play.dhis2.org/40/",
      username = "admin",
      password = "test"
    ),
      "Unable to authenticate due to an invalid username or password.
         Please update your credentials and try again.",
    fixed = TRUE #Checks against a regular expression so must be here
    )
  })
})
################################################################################
# TEST 2
################################################################################
context("Get a successful login")

httptest::with_mock_api({

  test_that("We get logged in successfully", {

    x <- loginToDATIM(
      base_url = "play.dhis2.org/40.0.1/",
      username = "admin",
      password = "district"
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
      base_url = "play.dhis2.org/badBaseUrl/",
      username = "admin",
      password = "test"
    ),
    "Unable to authenticate due to an invalid URL.Please check the
         'base_url' parameter you provided.",
    fixed = TRUE #Checks against a regular expression so must be here
    )
  })
})
################################################################################
# TEST 4
################################################################################
context("Get an unsuccessful login based upon DATIM currently undergoing
        maintenance")

httptest::with_mock_api({

  test_that("We cannot get logged in successfully due to DATIM currently
            undergoing maintenance", {

    testthat::expect_error(loginToDATIM(
      base_url = "play.dhis2.org/DatimMaintenance/",
      username = "admin",
      password = "test"
    ),
    "Unable to authenticate due to DATIM currently undergoing maintenance.
         Please try again later!",
    fixed = TRUE #Checks against a regular expression so must be here
    )
  })
})
################################################################################
# TEST 5
################################################################################
context("Get an unsuccessful login based upon DATIM having server issues")

httptest::with_mock_api({

  test_that("We cannot get logged in successfully due to DATIM servers being
            down", {

              testthat::expect_error(loginToDATIM(
                base_url = "play.dhis2.org/ServerDown/",
                username = "admin",
                password = "test"
              ),
              "Unable to reach DATIM, the server may be experiencing issues.
         Please try again later!",
         fixed = TRUE #Checks against a regular expression so must be here
              )
            })
})


################################################################################
# TEST 6
################################################################################
context("Get an unsuccessful login based upon an unaccounted for API response")

httptest::with_mock_api({

  test_that("We cannot get logged in successfully due to an unknown error", {

              testthat::expect_error(loginToDATIM(
                base_url = "play.dhis2.org/Unknown/",
                username = "admin",
                password = "test"
              ),
              "An unknowon error has occured during authentication!",
         fixed = TRUE  # Checks against a regular expression so must be here
              )
            })
})
