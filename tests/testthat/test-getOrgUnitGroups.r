context("make arbitrary api call with getorgunits")

with_mock_api({
  test_that("We can get a orgunit response", {
    base_url <- "www.datim.org/"
    filters <- "?filter=id:ilike:B"
    fields <- "name,id,numerator,denominator,categoryOptions"
    see <- getOrgUnitGroups(filters = c("Country", "Military"), by = "name",
                            fields = "name,id,organisationUnits[name,id,path]" )
    expect_identical(see$dataElements$id, "sAxSUTFc5tp")
  })
})