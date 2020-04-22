context("make arbitrary api call with getorgunits")

with_mock_api({
  test_that("We can get a orgunit response", {
    see <- getOrgUnitGroups(filters = c("Country", "Military"), 
                            fields = "name,id,organisationUnits[name,id,path]",
                            base_url = "www.datim.org/" ,
                            by = "name")
    expect_identical(see$organisationUnitGroups$name, "Country")
  })
})