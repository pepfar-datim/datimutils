test_that("convertFYQToCQ", {

  expect_equal(convertFYQToCQ(1995,1),"1994Q4")
  expect_equal(convertFYQToCQ(1995,2),"1995Q1")
  expect_equal(convertFYQToCQ(1995,3),"1995Q2")
  expect_equal(convertFYQToCQ(1995,4),"1995Q3")

})
