context("getMetadata")

# code to create/update mocks
#library(httptest)

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

test_that("Error handling ", {
  testthat::expect_error(
    getMetadata(indicators,
                fields = list(a = c("a", "a")),
                d2_session = play40.0.1))
  testthat::expect_error(
    getMetadata(d2_session = play40.0.1))
})

with_mock_api({
  test_that("Basic eq and sinlge element in call: ", {

# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/dataElements.json?",
#   "paging=false&filter=id:eq:FTRrcoaog83&fields=name,id")))
# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/dataElements.json?",
#   "paging=false&filter=id:in:[FTRrcoaog83]&fields=name,id")))

    data <- getMetadata(
      end_point = "dataElements",
      id %.eq% "FTRrcoaog83",
      d2_session = play40.0.1
    )
    # data <- data[["dataElements"]]
    testthat::expect_s3_class(data, "data.frame")
    testthat::expect_equal(NROW(data), 1)
    testthat::expect_named(data, c("name", "id"), ignore.order = TRUE)
    testthat::expect_equal(data$id, "FTRrcoaog83")
    expect_identical(data,
                     getMetadata(
                       end_point = "dataElements",
                       id %.in% "FTRrcoaog83",
                       d2_session = play40.0.1
                     ))
    rm(data)
  })

  test_that("getMetadata can handle operator of null, !null, empty, : ", {

# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/indicators.json?",
#   "paging=false&filter=code:null&fields=name,id")))
# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/indicators.json?",
#   "paging=false&filter=code:!null:&fields=name,id")))
# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/indicators.json?",
#   "paging=false&filter=legendSets:empty&fields=name,id")))
# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/indicators.json?",
#   "paging=false&filter=legendSets:empty:&fields=name,id")))
# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/indicators.json?",
#   "paging=false&filter=code:null:NA&fields=name,id")))
# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/indicators.json?",
#   "paging=false&filter=code:!null:NA&fields=name,id")))
# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/indicators.json?",
#   "paging=false&filter=legendSets:empty:NA&fields=name,id")))

    data <- getMetadata(end_point = "indicators",
                        "code:null",
                        d2_session = play40.0.1)
    testthat::expect_equal(NROW(data), 39)
    rm(data)
    data <- getMetadata(end_point = "indicators",
                        "code:!null:",
                        d2_session = play40.0.1)
    testthat::expect_named(data, c("name", "id"), ignore.order = TRUE)
    testthat::expect_equal(NROW(data), 38)
    rm(data)
    data <- getMetadata(end_point = "indicators",
                        "legendSets:empty",
                        d2_session = play40.0.1)
    testthat::expect_equal(NROW(data), 59)
    rm(data)
  })

  test_that("Single List Column becomes DF: ", {
    # paste0("List Columns: ",
    #        "https://play.dhis2.org/2.33/api/dimensions.json?",
    #        "paging=false&filter=id:eq:gtuVl6NbXQV",
    #        "&fields=items[name,id]"
    # )
    data <- getMetadata(
      end_point = "dimensions",
      "id:eq:gtuVl6NbXQV",
      d2_session = play40.0.1,
      fields = "items[name,id]"
    )
    testthat::expect_s3_class(data, "data.frame")
    testthat::expect_equal(NROW(data), 3)
    testthat::expect_named(data, c("name", "id"), ignore.order = TRUE)
    rm(data)

    # paste0("List Columns: ",
    #        "https://play.dhis2.org/2.33/api/dimensions.json?",
    #        "paging=false&filter=id:eq:gtuVl6NbXQV",
    #        "&fields=name,id,items[name,id]"
    # )
    data <- getMetadata(
      end_point = "dimensions",
      "id:eq:gtuVl6NbXQV",
      d2_session = play40.0.1,
      fields = "name,id,items[name,id]"
    )
    testthat::expect_s3_class(data, "data.frame")
    testthat::expect_equal(NROW(data), 1)
    testthat::expect_named(data, c("name", "id", "items"), ignore.order = TRUE)
    data <- tidyr::unnest(data, cols = items, names_sep = ".")
    testthat::expect_named(data, c(
      "name", "id",
      "items.name", "items.id"
    ), ignore.order = TRUE)
    testthat::expect_equal(NROW(data), 3)
    rm(data)

# httr::content(httr::GET(paste0("https://play.dhis2.org/2.33/api/dimensions.json?",
#        "paging=false&filter=id:eq:gtuVl6NbXQV",
#        "&fields=id,items[name]"
# )))

    data <- getMetadata(
      end_point = "dimensions",
      "id:eq:gtuVl6NbXQV",
      d2_session = play40.0.1,
      fields = "id,items[name]"
    )
    testthat::expect_s3_class(data, "data.frame")
    testthat::expect_equal(NROW(data), 1)
    testthat::expect_named(data, c("id", "items"), ignore.order = TRUE)
    data <- tidyr::unnest(data, cols = items, names_sep = ".")
    testthat::expect_named(data, c("id", "items.name"), ignore.order = TRUE)
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
      d2_session = play40.0.1,
      fields = "items[name,id]"
    )
    testthat::expect_s3_class(data, "data.frame")
    testthat::expect_equal(NROW(data), 7)
    testthat::expect_named(data, c("name", "id"), ignore.order = TRUE)
    rm(data)

    # paste0("List Columns: ",
    #        "https://play.dhis2.org/2.33/api/dimensions.json?",
    #        "paging=false&filter=id:in:[gtuVl6NbXQV,yY2bQYqNt0o]",
    #        "&fields=name,id,items[:all]"
    # )
    data <- getMetadata(
      end_point = "dimensions",
      "id:in:[gtuVl6NbXQV,yY2bQYqNt0o]",
      d2_session = play40.0.1,
      fields = "name,id,items[:all]"
    )
    testthat::expect_s3_class(data, "data.frame")
    testthat::expect_equal(NROW(data), 2)
    testthat::expect_named(data, c("name", "id", "items"), ignore.order = TRUE)
    data <- tidyr::unnest(data, cols = items, names_sep = ".")
    testthat::expect_gte(NCOL(data), 30)
    rm(data)

    data <- getMetadata(
      end_point = "indicators",
      "code:in:[IN_52462,IN_52486]",
      d2_session = play40.0.1,
      fields = ":all"
    )
    testthat::expect_s3_class(data, "data.frame")
    testthat::expect_equal(NROW(data), 2)
    testthat::expect_gte(NCOL(data), 41)
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
      id %.in% c("yY2bQYqNt0o", "gtuVl6NbXQV"),
      d2_session = play40.0.1,
      fields = "name,id,code"
    )
    testthat::expect_s3_class(data, "data.frame")
    testthat::expect_named(data,
                           c("code", "name", "id"),
                           ignore.order = TRUE)
    testthat::expect_equal(NROW(data), 2)
    rm(data)
  })

  test_that("No Filter: ", {

# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
#   "paging=false&fields=name,id")))

    data <- getMetadata(
      end_point = "organisationUnitGroups",
      d2_session = play40.0.1
    )
    # data <- data[["organisationUnitGroups"]]
    testthat::expect_equal(NROW(data), 18)
    rm(data)
  })

  test_that("as_vector parameter works: ", {

# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
#   "paging=false&fields=name")))
# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/organisationUnitGroupSets.json?",
#   "paging=false&fields=organisationUnitGroups[name]")))

    data <- getMetadata(
      end_point = "organisationUnitGroups",
      fields = "name",
      d2_session = play40.0.1
    )
    data2 <- getMetadata(
      end_point = "organisationUnitGroups",
      fields = "name",
      d2_session = play40.0.1,
      verbose = TRUE
    )
    testthat::expect_equal(NROW(data), 18)
    testthat::expect_named(data, NULL, ignore.order = TRUE)
    testthat::expect_named(data2$data, NULL, ignore.order = TRUE)
    rm(data)

    data <- getMetadata(
      end_point = "organisationUnitGroups",
      fields = "name",
      as_vector = FALSE,
      d2_session = play40.0.1
    )
    testthat::expect_equal(NROW(data), 18)
    testthat::expect_named(data, c("name"), ignore.order = TRUE)
    rm(data)

    data <- getMetadata(
      end_point = "organisationUnitGroupSets",
      fields = "organisationUnitGroups[name]",
      d2_session = play40.0.1)

    testthat::expect_equal(NROW(data), 15)
    testthat::expect_named(data, NULL, ignore.order = TRUE)
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
      organisationUnitGroups.id %.eq% "RpbiCJpIYEj",
      fields = "id,name,level,ancestors[id,name]",
      d2_session = play40.0.1
    )
    # data <- data[["organisationUnits"]]
    testthat::expect_equal(NROW(data), 1)
    testthat::expect_named(data, c("level", "name",
                                   "id", "ancestors"), ignore.order = TRUE)
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
    data <- getMetadata(end_point = "organisationUnits", c(
        "organisationUnitGroups.name:eq:District",
        "children.id:in:[YuQRtpLP10I,fwH9ipvXde9]"),
      fields = "id,name,level,ancestors[id,name]",
      d2_session = play40.0.1
    )
    testthat::expect_equal(NROW(data), 2)
    testthat::expect_named(data, c("level", "name",
                                   "id", "ancestors"), ignore.order = TRUE)
    rm(data)
    # filters sent as ...
    data <- getMetadata(
      end_point = "organisationUnits",
      "organisationUnitGroups.name:eq:District",
      "children.id:in:[YuQRtpLP10I,fwH9ipvXde9]",
      fields = "id,name,level,ancestors[id,name]",
      d2_session = play40.0.1
    )
    testthat::expect_equal(NROW(data), 2)
    testthat::expect_named(data, c("level", "name",
                                   "id", "ancestors"), ignore.order = TRUE)
    rm(data)
  })

  test_that("String like: ", {

# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/organisationUnits.json?",
#   "paging=false&filter=name:like:Baoma&fields=name,id")))

    data <- getMetadata(
      end_point = organisationUnits,
      name %.Like% "Baoma",
      d2_session = play40.0.1
    )
    testthat::expect_equal(NROW(data), 10)
    rm(data)
  })

  test_that("URL encoding: ", {

# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/organisationUnits.json?",
#   "paging=false&filter=name:like:Sierra%20Leone&fields=name,id")))

    data <- getMetadata(
      end_point = "organisationUnits",
      name %.Like% "Sierra Leone",
      d2_session = play40.0.1
    )

    testthat::expect_equal(NROW(data), 1)
    testthat::expect_named(data, c("name", "id"), ignore.order = TRUE)
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
      d2_session = play40.0.1
    )

    testthat::expect_equal(NROW(data), 1)
    testthat::expect_gte(NCOL(data), 38)

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
      d2_session = play40.0.1
    )

    testthat::expect_equal(NROW(data), 1)
    testthat::expect_named(data, c(
      "name", "id",
      "numerator", "denominator"
    ), ignore.order = TRUE)
  })

 #httr::content(httr::GET(paste0(
  #"https://play.dhis2.org/2.33/api/organisationUnits.json?",
  #"paging=false&filter=organisationUnitGroups.id:eq:RpbiCJpIYEj&fields=id")))
  test_that("Return atomic response", {
 data <- getMetadata(
      end_point = "organisationUnits",
      organisationUnitGroups.id %.eq% "RpbiCJpIYEj",
      fields = "id",
      d2_session = play40.0.1
    )
    testthat::expect_identical(data, "ImspTQPwCqd")
    rm(data)
  })

  test_that("Metadata filter format helpers", {

# standard call
    expect_identical(metadataFilter("V", "P", "O"),
                     "P:O:V")

# value null if and only if operator is null, !null or empty
    expect_identical(metadataFilter(NULL, "P", "null"),
                     "P:null:")
    expect_identical(metadataFilter(NULL, "P", "!null"),
                     "P:!null:")
    expect_identical(metadataFilter(NULL, "P", "empty"),
                     "P:empty:")
    expect_error(metadataFilter("V", "P", "null"))
    expect_error(metadataFilter("V", "P", "!null"))
    expect_error(metadataFilter("V", "P", "empty"))
    expect_error(metadataFilter(NULL, "P", "O"))

# values can have length > 1 only if operator is in or !in
# and values for in and !in always enclosed in square brackets
    expect_identical(metadataFilter(c(1, 2), "P", "in"),
                     "P:in:[1,2]")
    expect_identical(metadataFilter(c("1", "2"), "P", "!in"),
                     "P:!in:[1,2]")
    expect_identical(metadataFilter(1, "P", "in"),
                     "P:in:[1]")
    expect_identical(metadataFilter("2", "P", "!in"),
                     "P:!in:[2]")
    expect_error(metadataFilter(c("1", "2"), "P", "O"))

# standard and non standard eval
    expect_identical(id %.eq% "V",
                     "id" %.eq% "V")
    expect_identical(id %.in% c("V1", "V2"),
                     "id" %.in% c("V1", "V2"))

# %.in% and %.~in%
    expect_identical(P %.in% "V",
                     "P:in:[V]")
    expect_identical(P %.~in% "V",
                     "P:!in:[V]")
    expect_identical(P %.in% c("V_1", "V_2"),
                     "P:in:[V_1,V_2]")
    expect_identical(P %.~in% c("V_1", "V_2"),
                     "P:!in:[V_1,V_2]")

# %.eq% and %.~eq%
    expect_identical(P %.eq% "V", "P:eq:V")
    expect_identical(P %.~eq% "V", "P:!eq:V")

    # %.like% and %.~like%
    expect_identical(P %.like% "V", "P:ilike:V")
    expect_identical(P %.~like% "V", "P:!ilike:V")

    # %.like$% and %.~like$%
    expect_identical(P %.like$% "V", "P:ilike$:V")
    expect_identical(P %.~like$% "V", "P:!ilike$:V")

    # %.^like% and %.~^like%
    expect_identical(P %.^like% "V", "P:!ilike:V")
    expect_identical(P %.~^like% "V", "P:!$ilike:V")

    # %.Like$% and %.~Like$%
    expect_identical(P %.Like$% "V", "P:like$:V")
    expect_identical(P %.~Like$% "V", "P:!like$:V")

    # %.^Like% and %.~^Like%
    expect_identical(P %.^Like% "V", "P:$like:V")
    expect_identical(P %.~^Like% "V", "P:!$like:V")

    # %.token% and %.~token%
    expect_identical(P %.token% "V", "P:token:V")
    expect_identical(P %.~token% "V", "P:!token:V")

    # %.le%, %.It% %.ge% %.gt% and %.~.Like%
    expect_identical(P %.le% "V", "P:le:V")
    expect_identical(P %.lt% "V", "P:lt:V")
    expect_identical(P %.ge% "V", "P:ge:V")
    expect_identical(P %.gt% "V", "P:gt:V")
    expect_identical(P %.~Like$% "V", "P:!like$:V")
    expect_identical(P %.~Like% "V", "P:!like:V")


    })
  })
