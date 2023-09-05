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
    data <- getDataValueSets(variable_keys = c("dataSet", "period", "orgUnit"),
                             variable_values = c("pBOMPrpg1QX", "202201", "DiszpKrYNg8"),
                             d2_session = play40.0.1)

    testthat::expect_named(data, c("dataElement",
                                   "period",
                                   "orgUnit",
                                   "categoryOptionCombo",
                                   "attributeOptionCombo",
                                   "value",
                                   "storedBy",
                                   "created",
                                   "lastUpdated",
                                   "comment",
                                   "followup"))
    testthat::expect_equal(NROW(data), 4)

    data2 <- getDataValueSets(variable_keys = c("dataSet", "period", "orgUnit"),
                              variable_values = c("pBOMPrpg1QX", "202201", "DiszpKrYNg8"),
                              d2_session = play40.0.1,
                              verbose = TRUE)
    testthat::expect_type(data2, "list")
    testthat::expect_named(data2$data, c("dataElement",
                                         "period",
                                         "orgUnit",
                                         "categoryOptionCombo",
                                         "attributeOptionCombo",
                                         "value",
                                         "storedBy",
                                         "created",
                                         "lastUpdated",
                                         "comment",
                                         "followup"))
    testthat::expect_equal(NROW(data2$data), 4)

    # test missing data set or data element group
    testthat::expect_error(getDataValueSets(variable_keys = c("limit"),
                                            variable_values = c("3"),
                                            d2_session = play40.0.1),
                           "At least one data set or data element group must be specified.",
                           fixed = TRUE)

    # test missing orgunit or orgunit group
    testthat::expect_error(getDataValueSets(variable_keys = c("dataSet", "period"),
                                            variable_values = c("pBOMPrpg1QX", "202201"),
                                            d2_session = play40.0.1),
                           "At least one organisation unit or organisation unit group must be specified.",
                           fixed = TRUE)


  })
})
