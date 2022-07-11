context("Test GetDataValueSets")

test_that("GetDataValueSets", {
  # httptest::start_capturing(simplify = FALSE)
  # httr::content(httr::GET(paste0(
  #   "https://play.dhis2.org/2.37.2/api/dataValueSets.json?",
  #   "dataSet=pBOMPrpg1QX&period=202201&orgUnit=DiszpKrYNg8&paging=false"),
  #   httr::authenticate("admin", "district")))
  # httptest::stop_capturing()
  #Note didn't work due to api_get, so just copied and pasted json to 
  #correct file
  
  with_mock_api({
    data <- getDataValueSets(c("dataSet", "period", "orgUnit"),
                             c("pBOMPrpg1QX", "202201", "DiszpKrYNg8"),
                             d2_session = play2372)
  
  
    testthat::expect_named(data, c("data_element",
                                   "period",
                                   "org_unit",
                                   "category_option_combo",
                                   "attribute_option_combo",
                                   "value",
                                   "stored_by",
                                   "created",
                                   "last_updated",
                                   #"comment",
                                   "followup"))
    testthat::expect_equal(NROW(data), 3)
    testthat::expect_error(getDataValueSets(c("limit"),
                                            c("3"),
                                            play2372),
                           "At least one data set must be specified.",
                           fixed=TRUE)
  })
})