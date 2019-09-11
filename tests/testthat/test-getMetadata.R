test_that("formatForApi_filters", {
  parameters <- tibble::tribble (~property, ~operator, ~value,
                                 "name", "ilike", "sierra",
                                 "id", "in", c("11111111111","22222222222"),
                                 "id", "in", "22222222222",
                                 "code", "!null", NA,
                                 "code", "!null", "",
                                 "code", "null", NULL)
  r <- formatForApi_filters(parameters)
  testthat::expect_identical(r,
                             "&filter=name:ilike:sierra&filter=id:in:[11111111111,22222222222]&filter=id:in:[22222222222]&filter=code:!null&filter=code:!null&filter=code:null")
  parameters <- dplyr::select(parameters, -property)
  testthat::expect_error(formatForApi_filters(parameters))
})

test_that("retryAPI", {
  retryAPI("http://httpbin.org/json", "application/json") %>% 
    httr::status_code() %>% 
    testthat::expect_equal(200)
   
    testthat::expect_error(
      retryAPI("http://httpbin.org/json", 
               "application/html", 
               max_attempts = 1))
    
    testthat::expect_error(
      retryAPI("https://httpbin.org/status/403", 
               "text/html", 
               max_attempts = 1))
    
})
