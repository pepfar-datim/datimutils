context("make arbitrary api call with getorgunitgroups")

# code to create/update mocks
# httptest::start_capturing(simplify = FALSE)
# #not logged in for this one, fields there just to give the call a unique httptest ID
# httr::GET("https://play.dhis2.org/2.33/api/me.json?paging=false&fields=notloggedin")
# httptest::stop_capturing()

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
                   "&fields=name,id,organisationUnits[name,id]"),
            httr::authenticate("admin", "district"))
)
datimutils::getOrgUnitGroups(c("CHP", "Rural"),
                             by = "name",
                             fields = "name,id,organisationUnits[name,id]",
                             base_url = "https://play.dhis2.org/2.33/")
# httptest::start_capturing(simplify = FALSE)

with_mock_api({
  test_that("We can get a orgunit response", {
    see <- getOrgUnitGroups(x = c("Country", "Military"),
                            by = "name",
                            fields = "name,id,organisationUnits[name,id,path]",
                            base_url = "www.datim.org/")
    expect_identical(see$organisationUnitGroups$name, "Country")
  })
})
