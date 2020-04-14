context("make arbitrary api call DATIM")

with_mock_api({
  test_that("We can get a metadataresponse", {
    aseurl = "www.datim.org/" 
    end_point = "dataElements" 
    filters <- "?filter=id:ilike:B" 
    fields <- "name,id,numerator,denominator,categoryOptions" 
    see <- getMetadata(baseurl = baseurl, end_point = end_point, filters = filters, fields = fields)
    expect_identical(see$dataElements$id, "sAxSUTFc5tp")
  })
})
