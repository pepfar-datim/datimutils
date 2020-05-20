context("make arbitrary api call with getorgunitgroups2")

# code to create/update mocks
library(httptest)

#httptest::start_capturing(simplify = FALSE)
# httr::content(
#   httr::GET(paste0("https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
#                  "paging=false&filter=id:in:[CXw2yu5fodb,gzcv65VyaGq]",
#                  "&filter=groupSets.id:in:[J5jldMd8OHv]&fields=code"),
#           httr::authenticate("admin", "district"))
# )
#httptest::stop_capturing()

httptest::with_mock_api({
test_that(
  paste0("https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
         "paging=false&filter=id:in:[CXw2yu5fodb,gzcv65VyaGq]",
         "&filter=groupSets.id:in:[J5jldMd8OHv]&fields=code"), {
           
           data <- datimutils::getOrgUnitGroups2(c("CXw2yu5fodb","gzcv65VyaGq"),
                                                 "J5jldMd8OHv",
                                                 "id",
                                                 "groupSets.id",
                                                 c("code"),
                                                 "https://play.dhis2.org/2.33/")
           #data <- tibble::as_tibble(data$organisationUnitGroups)
           testthat::expect_s3_class(data, "data.frame")
           testthat::expect_equal(NROW(data), 1)
           testthat::expect_equal(data$code, "CHC")
           rm(data)
           })
  })