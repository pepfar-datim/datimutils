context("getMetadata")

# code to create/update mocks
library(httptest)

# httptest::start_capturing(simplify = FALSE)
# httr::content(
#   httr::GET(
#     paste0("https://play.dhis2.org/2.33/api/indicators.json?",
#            "paging=false&filter=id:eq:ReUHfIn0pTQ",
#            "&fields=name,id,numerator,denominator"
#            ),
#     httr::authenticate("admin", "district")
#     )
#   )
# 
# httptest::stop_capturing()

# with_mock_api({

   httr::GET(
     paste0("https://play.dhis2.org/2.33/api/me"),
    httr::authenticate("admin", "district")
    )
   
   
   test_that(paste0("https://play.dhis2.org/2.33/api/dataElements.json?",
                    "paging=false&filter=id:eq:D7Ehxtgosna"
                    ), {
     data <- getMetadata(
       end_point = "dataElements",
       filters = "id:eq:D7Ehxtgosna", 
       base_url = "https://play.dhis2.org/2.33/"
     )
     #data <- data[["dataElements"]]
     testthat::expect_s3_class(data, "data.frame")
     testthat::expect_equal(NROW(data), 1)
     testthat::expect_named(data, c("id", "displayName"))
     testthat::expect_equal(data$id, "D7Ehxtgosna")
     rm(data)
   })
   
   test_that(paste0("https://play.dhis2.org/2.33/api/dimensions.json?",
                    "paging=false&filter=id:eq:gtuVl6NbXQV",
                    "&fields=items[name,id]"
   ), {
     data <- getMetadata(
       end_point = "dimensions",
       filters = "id:eq:gtuVl6NbXQV", 
       base_url = "https://play.dhis2.org/2.33/",
       fields = "items[name,id]"
     )
     #data <- data[["dimensions"]]
     testthat::expect_s3_class(data, "data.frame")
     testthat::expect_equal(NROW(data), 1)
     testthat::expect_named(data, "items")
     data <- tidyr::unnest(data, cols = items)
     testthat::expect_named(data, c("name", "id"))
     testthat::expect_equal(NROW(data), 3)
     rm(data)
   })
      
  test_that(paste0("https://play.dhis2.org/2.33/api/indicators.json?",
                   "paging=false&filter=id:eq:ReUHfIn0pTQ",
                   "&fields=name,id,numerator,denominator"
                   ), {
                     data <- getMetadata(
                       end_point = "indicators",
                       filters = "id:eq:ReUHfIn0pTQ", 
                       fields = "name,id,numerator,denominator",
                       base_url = "https://play.dhis2.org/2.33/"
                     )
                   })
  
  
# })