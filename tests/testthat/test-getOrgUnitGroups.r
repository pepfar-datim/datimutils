context("make arbitrary api call with getorgunits")

with_mock_api({
  test_that("We can get a orgunit response", {
    see <- getOrgUnitGroups(x = c("Country", "Military"), 
                            by = "name",
                            fields = "name,id,organisationUnits[name,id,path]",
                            base_url = "www.datim.org/")
    expect_identical(see$organisationUnitGroups$name, "Country")
  })
})