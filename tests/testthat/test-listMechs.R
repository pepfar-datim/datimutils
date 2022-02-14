# test for appropriate columnns -----
httptest::with_mock_api({
  test_that("test that appropriate columns are returned...", {
    
    # data and benchmark
    data <- listMechs(d2_session = test)
    columns <- c("mech_code","name","option_id","combo_id")
    
    # test equvalence
    testthat::expect_equal(
      names(data),
      columns
    )
    
    #rm data
    rm(data, columns)
    
  })
})

# test that the right mech list count is returned ----
httptest::with_mock_api({
test_that("test should return less than 3000 mechs for this user...", {
  
  # pull data, compare, clean
  data <- listMechs(d2_session = test)
  testthat:::expect_lt(nrow(data), 8000)
  rm(data)
  
  })
})

# test the error message with wrong options fields ----
httptest::with_mock_api({
  test_that("testing error on option fields...", {
    
    # expect error with invalid ids
    expect_error(
      listMechs(
        option_fields = c("name","ids", "code"),
        d2_session = test
      ),
      "other fields are currently not supported."
    )
    
  })
})


# test the error message with wrong combo fields ----
httptest::with_mock_api({
  test_that("testing error on combo field...", {
    
    # expect error with invalid ids
    expect_error(
      listMechs(
        combo_fields = "ids",
        d2_session = test
      ),
      "other fields are currently not supported."
    )
    
  })
})






