context("getMetadata")

# code to create/update mocks
library(httptest)

# httptest::start_capturing(simplify = FALSE)
# httr::content(
#   httr::GET(
#     paste0(
#       "https://play.dhis2.org/2.33/api/dataElements.json?",
#       "paging=false&filter=id:eq:FTRrcoaog83"
#     ),
#     httr::authenticate("admin", "district")
#   )
# )
# httr::content(
#   httr::GET(
#     paste0("https://play.dhis2.org/2.33/api/dimensions.json?",
#            "paging=false&filter=id:eq:gtuVl6NbXQV",
#            "&fields=items[name,id]"
#     ),
#     httr::authenticate("admin", "district")
#   )
# )
# httr::content(
#   httr::GET(
#     paste0("https://play.dhis2.org/2.33/api/dimensions.json?",
#            "paging=false&filter=id:eq:gtuVl6NbXQV",
#            "&fields=name,id,items[name,id]"
#     ),
#     httr::authenticate("admin", "district")
#   )
# )
# httr::content(
#   httr::GET(
#     paste0("https://play.dhis2.org/2.33/api/dimensions.json?",
#            "paging=false&filter=id:in:[gtuVl6NbXQV,yY2bQYqNt0o]",
#            "&fields=items[name,id]"
#     ),
#     httr::authenticate("admin", "district")
#   ))
# httr::content(
#   httr::GET(
#     paste0("https://play.dhis2.org/2.33/api/dimensions.json?",
#            "paging=false&filter=id:in:[gtuVl6NbXQV,yY2bQYqNt0o]",
#            "&fields=name,id,items[:all]"
#     ),
#     httr::authenticate("admin", "district")
#   ))
# httr::content(
#   httr::GET(
#     paste0("https://play.dhis2.org/2.33/api/indicators.json?",
#            "paging=false&filter=code:in:[IN_52462,IN_52486]",
#            "&fields=:all"
#     ),
#     httr::authenticate("admin", "district")
#   ))
# httr::content(
#   httr::GET(
#     paste0(
#       "https://play.dhis2.org/2.33/api/dimensions.json?",
#       "paging=false&filter=id:in:[yY2bQYqNt0o,gtuVl6NbXQV]",
#       "&fields=name,id,code"
#     ),
#     httr::authenticate("admin", "district")
#   ))
# httr::content(
#   httr::GET(
#     paste0("https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
#            "paging=false"),
#     httr::authenticate("admin", "district")
#   ))
# httr::content(
#   httr::GET(
#     paste0("https://play.dhis2.org/2.33/api/organisationUnits.json?",
#            "paging=false",
#            "&filter=organisationUnitGroups.id:eq:RpbiCJpIYEj",
#            "&fields=id,name,level,ancestors[id,name]"),
#     httr::authenticate("admin", "district")
#   ))
# httr::content(
#   httr::GET(
#     paste0("https://play.dhis2.org/2.33/api/organisationUnits.json?",
#            "paging=false",
#            "&filter=organisationUnitGroups.name:eq:District",
#            "&filter=children.id:in:[YuQRtpLP10I,fwH9ipvXde9]",
#            "&fields=id,name,level,ancestors[id,name]"),
#     httr::authenticate("admin", "district")
#   )
# )
# httr::content(
#   httr::GET(
#     paste0("https://play.dhis2.org/2.33/api/organisationUnits.json?",
#            "paging=false&filter=name:like:Baoma"
#     ),
#     httr::authenticate("admin", "district")
#   )
# )
# httr::content(
#   httr::GET(
#     paste0("https://play.dhis2.org/2.33/api/organisationUnits.json?",
#            "paging=false&filter=name:like:Baoma",
#            "&filter=level:eq:3&fields=:all"),
#     httr::authenticate("admin", "district")
#   )
# )
# httr::content(
#   httr::GET(
#     paste0("https://play.dhis2.org/2.33/api/indicators.json?",
#            "paging=false&filter=id:eq:ReUHfIn0pTQ",
#            "&fields=name,id,numerator,denominator"
#     ),
#     httr::authenticate("admin", "district")
#   )
# )
#
# httr::content(
#   httr::GET(
#     paste0("https://play.dhis2.org/2.33/api/organisationUnits.json?",
#            "paging=false&filter=name:like:Sierra%20Leone"
#     ),
#     httr::authenticate("admin", "district")
#   )
# )
# httptest::stop_capturing()

with_mock_api({
  test_that("Basic eq call: ", {
  
# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/dataElements.json?",
#   "paging=false&filter=id:eq:FTRrcoaog83&fields=name,id")))
    
    data <- getMetadata(
      end_point = "dataElements",
      id %deq% "FTRrcoaog83",
      base_url = "https://play.dhis2.org/2.33/"
    )
    # data <- data[["dataElements"]]
    testthat::expect_s3_class(data, "data.frame")
    testthat::expect_equal(NROW(data), 1)
    testthat::expect_named(data, c("name", "id"))
    testthat::expect_equal(data$id, "FTRrcoaog83")
    rm(data)
  })

  test_that("List Columns: ", {
    # paste0("List Columns: ",
    #        "https://play.dhis2.org/2.33/api/dimensions.json?",
    #        "paging=false&filter=id:eq:gtuVl6NbXQV",
    #        "&fields=items[name,id]"
    # )
    data <- getMetadata(
      end_point = "dimensions",
      "id:eq:gtuVl6NbXQV",
      base_url = "https://play.dhis2.org/2.33/",
      fields = "items[name,id]"
    )
    testthat::expect_s3_class(data, "data.frame")
    testthat::expect_equal(NROW(data), 3)
    testthat::expect_named(data, c("name", "id"))
    rm(data)

    # paste0("List Columns: ",
    #        "https://play.dhis2.org/2.33/api/dimensions.json?",
    #        "paging=false&filter=id:eq:gtuVl6NbXQV",
    #        "&fields=name,id,items[name,id]"
    # )
    data <- getMetadata(
      end_point = "dimensions",
      "id:eq:gtuVl6NbXQV",
      base_url = "https://play.dhis2.org/2.33/",
      fields = "name,id,items[name,id]"
    )
    testthat::expect_s3_class(data, "data.frame")
    testthat::expect_equal(NROW(data), 1)
    testthat::expect_named(data, c("name", "id", "items"))
    data <- tidyr::unnest(data, cols = items, names_sep = ".")
    testthat::expect_named(data, c(
      "name", "id",
      "items.name", "items.id"
    ))
    testthat::expect_equal(NROW(data), 3)
    rm(data)

    # paste0("List Columns: ",
    #        "https://play.dhis2.org/2.33/api/dimensions.json?",
    #        "paging=false&filter=id:in:[gtuVl6NbXQV,yY2bQYqNt0o]",
    #        "&fields=items[name,id]"
    # )
    data <- getMetadata(
      end_point = "dimensions",
      "id:in:[gtuVl6NbXQV,yY2bQYqNt0o]",
      base_url = "https://play.dhis2.org/2.33/",
      fields = "items[name,id]"
    )
    testthat::expect_s3_class(data, "data.frame")
    testthat::expect_equal(NROW(data), 7)
    testthat::expect_named(data, c("name", "id"))
    rm(data)

    # paste0("List Columns: ",
    #        "https://play.dhis2.org/2.33/api/dimensions.json?",
    #        "paging=false&filter=id:in:[gtuVl6NbXQV,yY2bQYqNt0o]",
    #        "&fields=name,id,items[:all]"
    # )
    data <- getMetadata(
      end_point = "dimensions",
      "id:in:[gtuVl6NbXQV,yY2bQYqNt0o]",
      base_url = "https://play.dhis2.org/2.33/",
      fields = "name,id,items[:all]"
    )
    testthat::expect_s3_class(data, "data.frame")
    testthat::expect_equal(NROW(data), 2)
    testthat::expect_named(data, c("name", "id", "items"))
    data <- tidyr::unnest(data, cols = items, names_sep = ".")
    testthat::expect_equal(NCOL(data), 30)
    rm(data)

    data <- getMetadata(
      end_point = "indicators",
      "code:in:[IN_52462,IN_52486]",
      base_url = "https://play.dhis2.org/2.33/",
      fields = ":all"
    )
    testthat::expect_s3_class(data, "data.frame")
    testthat::expect_equal(NROW(data), 2)
    testthat::expect_equal(NCOL(data), 41)
    rm(data)
  })

  test_that(paste0(
    "Basic in call: ",
    "https://play.dhis2.org/2.33/api/dimensions.json?",
    "paging=false&filter=id:in:[yY2bQYqNt0o,gtuVl6NbXQV]",
    "&fields=name,id,code"
  ), {
    data <- getMetadata(
      end_point = "dimensions",
      id %din% c("yY2bQYqNt0o", "gtuVl6NbXQV"),
      base_url = "https://play.dhis2.org/2.33/",
      fields = "name,id,code"
    )
    testthat::expect_s3_class(data, "data.frame")
    testthat::expect_named(data, c("code", "name", "id"))
    testthat::expect_equal(NROW(data), 2)
    rm(data)
  })

  test_that("No Filter: ", {

# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
#   "paging=false&fields=name,id")))
    
    data <- getMetadata(
      end_point = "organisationUnitGroups",
      base_url = "https://play.dhis2.org/2.33/"
    )
    # data <- data[["organisationUnitGroups"]]
    testthat::expect_equal(NROW(data), 18)
    rm(data)
  })

  test_that(paste0(
    "Filter with path: ",
    "https://play.dhis2.org/2.33/api/organisationUnits.json?",
    "paging=false",
    "&filter=organisationUnitGroups.id:eq:RpbiCJpIYEj",
    "&fields=id,name,level,ancestors[id,name]"
  ), {
    data <- getMetadata(
      end_point = "organisationUnits",
      organisationUnitGroups.id %deq% "RpbiCJpIYEj",
      fields = "id,name,level,ancestors[id,name]",
      base_url = "https://play.dhis2.org/2.33/"
    )
    # data <- data[["organisationUnits"]]
    testthat::expect_equal(NROW(data), 1)
    rm(data)
  })

  test_that(paste0(
    "Two Filters: ",
    "https://play.dhis2.org/2.33/api/organisationUnits.json?",
    "paging=false",
    "&filter=organisationUnitGroups.name:eq:District",
    "&filter=children.id:in:[YuQRtpLP10I,fwH9ipvXde9]",
    "&fields=id,name,level,ancestors[id,name]"
  ), {
    # filters sent as list
    data <- getMetadata(
      end_point = "organisationUnits",
      c(
        "organisationUnitGroups.name:eq:District",
        "children.id:in:[YuQRtpLP10I,fwH9ipvXde9]"
      ),
      fields = "id,name,level,ancestors[id,name]",
      base_url = "https://play.dhis2.org/2.33/"
    )
    testthat::expect_equal(NROW(data), 2)
    rm(data)
    # filters sent as ...
    data <- getMetadata(
      end_point = "organisationUnits",
      "organisationUnitGroups.name:eq:District",
      "children.id:in:[YuQRtpLP10I,fwH9ipvXde9]",
      fields = "id,name,level,ancestors[id,name]",
      base_url = "https://play.dhis2.org/2.33/"
    )
    testthat::expect_equal(NROW(data), 2)
    rm(data)
  })

  test_that("String like: ", {
    
# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/organisationUnits.json?",
#   "paging=false&filter=name:like:Baoma&fields=name,id")))
    
    data <- getMetadata(
      end_point = organisationUnits,
      name %dlike% "Baoma",
      base_url = "https://play.dhis2.org/2.33/"
    )
    testthat::expect_equal(NROW(data), 10)
    rm(data)
  })

  test_that("URL encoding: ",{
    
# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/organisationUnits.json?",
#   "paging=false&filter=name:like:Sierra%20Leone&fields=name,id")))

    data <- getMetadata(
      end_point = "organisationUnits",
      name %dlike% "Sierra Leone",
      base_url = "https://play.dhis2.org/2.33/"
    )
    testthat::expect_equal(NROW(data), 1)
    rm(data)
  })

  test_that(paste0(
    "Filter with less common property: ",
    "https://play.dhis2.org/2.33/api/organisationUnits.json?",
    "paging=false&filter=name:like:Baoma",
    "&filter=level:eq:3&fields=:all"
  ), {
    data <- getMetadata(
      end_point = "organisationUnits",
      c(
        "name:like:Baoma",
        "level:eq:3"
      ),
      fields = ":all",
      base_url = "https://play.dhis2.org/2.33/"
    )

    # testthat::expect_s3_class(data, "data.frame")
    testthat::expect_equal(NROW(data), 1)
    # testthat::expect_named(data, "items")
    # data <- tidyr::unnest(data, cols = items)
    # testthat::expect_named(data, c("name", "id"))
    # testthat::expect_equal(NROW(data), 7)
    rm(data)
  })


  test_that(paste0(
    "Call with indicator endpoint: ",
    "https://play.dhis2.org/2.33/api/indicators.json?",
    "paging=false&filter=id:eq:ReUHfIn0pTQ",
    "&fields=name,id,numerator,denominator"
  ), {
    data <- getMetadata(
      end_point = "indicators",
      "id:eq:ReUHfIn0pTQ",
      fields = "name,id,numerator,denominator",
      base_url = "https://play.dhis2.org/2.33/"
    )

    expect_equal(NROW(data), 1)
    expect_named(data, c(
      "name", "id",
      "numerator", "denominator"
    ))
  })
})
