library(httptest)
library(httr)
library(xml2)
library(datimutils)

app <- oauth_app("Shiny App Datimutils", # dhis2 = Name
                 key = "Shiny App Datimutils",# dhis2 = Client ID
                 secret = "e74a0ced9-946d-8e9b-2b31-2a6b330f36e",# dhis2 = Client Secret
                 redirect_uri = "http://127.0.0.1:8100/" 
)

api <- oauth_endpoint(base_url = "https://cop-test.datim.org/uaa/oauth",
                      request=NULL,# Documentation says to leave this NULL for OAuth2 
                      authorize = "authorize",
                      access="token"
) 

scope <- "ALL"

### Set options
options(httr_oob_default=TRUE)

token <- readRDS("~/Documents/Repos/datimutils/tests/testthat/token.rds")

#capture_requests({

# Unsuccessful based on credentials.
# loginToDATIMOAuth(
#   base_url = 'play.dhis2.org/2.372/',
#   username = 'admin',
#   password = 'test')
# Succesful
# loginToDATIMOAuth(
#     base_url = 'play.dhis2.org/2.37/',
#     username = 'admin',
#     password = 'district')

# Unsuccessful based on bad base_url.
# loginToDATIMOAuth(
#    base_url = 'play.dhis2.org/badBaseUrl/',
#    username = 'admin',
#    password = 'district')

#})

################################################################################
# TEST 1
################################################################################
context("Fail cross authentication")

httptest::with_mock_api({

  test_that("We authenticated with one server and tried to retrieve data from another", {

    testthat::expect_error(loginToDATIMOAuth(
      base_url = "play.dhis2.org/2.372/",
      token = token,
      app=app, api = api, redirect_uri="http://127.0.0.1:8100/", scope = scope
    ),
    "cop-test.datim.org/uaa/oauth/token"#,
    #fixed = TRUE #Checks against a regular expression so must be here
    )
  })

})

################################################################################
# TEST 2
################################################################################
context("Fail to get token without authorization code")

httptest::with_mock_api({
  
  test_that("We failed to get an access token without authorization code", {
    
    testthat::expect_error(
      getOAuthToken("http://127.0.0.1:8100/",
                    app,
                    api, 
                    scope),
    "Can only use oob authentication in an interactive session",
    fixed = TRUE #Checks against a regular expression so must be here
    )
  })
  
})

################################################################################
# TEST 3
################################################################################
context("Get a successful login")

httptest::with_mock_api({

  test_that("We get logged in successfully", {

    x <- loginToDATIMOAuth(
      base_url = "play.dhis2.org/2.37/",
      token = token,
      app=app, api = api, redirect_uri="http://127.0.0.1:8100/", scope = scope
    )

    testthat::expect_identical(x$me$userCredentials$username, "admin")

  })
})

