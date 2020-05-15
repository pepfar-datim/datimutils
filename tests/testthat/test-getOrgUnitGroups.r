context("make arbitrary api call with getorgunitgroups")

# code to create/update mocks
library(httptest)

#httptest::start_capturing(simplify = FALSE)
httr::content(
  httr::GET(paste0("https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
                 "paging=false&filter=id:in:[CXw2yu5fodb]&fields=name,id"),
          httr::authenticate("admin", "district"))
)
datimutils::getOrgUnitGroups("CXw2yu5fodb", 
                             base_url = "https://play.dhis2.org/2.33/")

httr::content(
  httr::GET(paste0("https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
                 "paging=false&filter=name:in:[CHP,Rural]",
                 "&fields=id,code"),
          httr::authenticate("admin", "district"))
)
datimutils::getOrgUnitGroups(c("CHP", "Rural"),
                             by = "name",
                             fields = c("id", "code"),
                             base_url = "https://play.dhis2.org/2.33/")

httr::content(
  httr::GET(paste0("https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
                   "paging=false&filter=name:in:[CHP,Rural]",
                   "&fields=name,id,organisationUnits[name,id],groupSets[name,id]"),
            httr::authenticate("admin", "district"))
)
datimutils::getOrgUnitGroups(
  c("CHP", "Rural"),
  by = "name",
  fields = "name,id,organisationUnits[name,id],groupSets[name,id]",
  base_url = "https://play.dhis2.org/2.33/")

#httptest::stop_capturing()

test_that(
  paste0("https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
         "paging=false&filter=id:in:[CXw2yu5fodb]&fields=name,id"), {
           
           data <- datimutils::getOrgUnitGroups("CXw2yu5fodb", 
                                        base_url = "https://play.dhis2.org/2.33/")
           #data <- tibble::as_tibble(data$organisationUnitGroups)
           testthat::expect_s3_class(data, "data.frame")
           testthat::expect_equal(NROW(data), 1)
           testthat::expect_named(data, c("name", "id"))
           testthat::expect_equal(data$id, "CXw2yu5fodb")
           rm(data)
})

test_that("Uses default base_url", {
    original_baseurl = getOption("baseurl")
    options("baseurl" = "https://play.dhis2.org/2.33/")
    data <- datimutils::getOrgUnitGroups("CXw2yu5fodb")
    testthat::expect_equal(data$id, "CXw2yu5fodb")
    options("baseurl" = original_base_url)
    rm(data)
  })

test_that(
  paste0("https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
         "paging=false&filter=name:in:[CHP,Rural]",
         "&fields=id,code"), {
           
           data <- 
             datimutils::getOrgUnitGroups(
               c("CHP", "Rural"),
               by = "name",
               fields = c("id", "code"),
               base_url = "https://play.dhis2.org/2.33/"
               )
           
           #data <- tibble::as_tibble(data$organisationUnitGroups)
           testthat::expect_equal(NROW(data), 2)
           testthat::expect_named(data, c("code", "id"))
           #testthat::expect_equal(data$id, "CXw2yu5fodb")
           rm(data)
         })

test_that(
  paste0("https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
         "paging=false&filter=name:in:[CHP,Rural]",
         "&fields=name,id,organisationUnits[name,id],groupSets[name,id]"), {
           
           data <-
             datimutils::getOrgUnitGroups(
               c("CHP", "Rural"),
               by = "name",
               fields = c("name", "id", "organisationUnits[name,id]",
                          "groupSets[name,id]"),
               base_url = "https://play.dhis2.org/2.33/"
             )
           
           #data <- tibble::as_tibble(data$organisationUnitGroups)
           testthat::expect_s3_class(data, "data.frame")
           testthat::expect_equal(NROW(data), 2)
           #testthat::expect_named(data, c("code", "id"))
           #testthat::expect_equal(data$id, "CXw2yu5fodb")
           rm(data)
         })
