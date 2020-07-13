context("make arbitrary api call with getorgunitgroups")

# code to create/update mocks
# library(httptest)
#
# httptest::start_capturing(simplify = FALSE)
# httr::content(
#   httr::GET(paste0("https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
#                  "paging=false&filter=id:in:[CXw2yu5fodb]&fields=name,id"),
#           httr::authenticate("admin", "district"))
# )
#
# httr::content(
#   httr::GET(paste0("https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
#                  "paging=false&filter=name:in:[CHP,Rural]",
#                  "&fields=id,code"),
#           httr::authenticate("admin", "district"))
# )
#
# httr::content(
#   httr::GET(paste0("https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
#                    "paging=false&filter=name:in:[CHP,Rural]",
#                    "&fields=name,id,organisationUnits[name,id],groupSets[name,id]"),
#             httr::authenticate("admin", "district"))
# )
#
# httr::GET(paste0("https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
#        "paging=false&filter=id:in:[gzcv65VyaGq,uYxK4wmcPqA,",
#        "RXL3lPSK8oG,RpbiCJpIYEj,w1Atoz18PCL,CXw2yu5fodb]",
#        "&fields=code,name,id"))
#
# httptest::stop_capturing()

httptest::with_mock_api({
  test_that(paste0("Default behavior, given id return name"), {

# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
#   "paging=false&filter=id:in:[CXw2yu5fodb]&fields=name,id")))
# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
#   "paging=false&filter=id:in:[CXw2yu5fodb]&fields=name")))
# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
#   "paging=false&filter=id:in:[CXw2yu5fodb]&fields=id,name")))

      data <- getOrgUnitGroups(
        "CXw2yu5fodb",
        base_url = "https://play.dhis2.org/2.33/")
      testthat::expect_equal(data, "CHC")
      rm(data)
      })



  
  test_that(
    paste0("Default behavior, given name return id (using standard",
           "evaluation of by): "), {
             
# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
#   "paging=false&filter=name:in:[CHC]&fields=id,name")))
             
      data <- getOrgUnitGroups(
        "CHC", by = "name",
        base_url = "https://play.dhis2.org/2.33/")
      
      testthat::expect_equal(data, "CXw2yu5fodb")
      rm(data)
    }
  )
  
  test_that(
    paste0("Default behavior, provide name get back id ", 
           "(non standard evaluation of by):"
           ), {
             
# httr::content(httr::GET(
#   paste0("https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
#          "paging=false&filter=name:in:[CHC]&fields=name,id")))
      
             data <- getOrgUnitGroups(
               "CHC", by = name,
               base_url = "https://play.dhis2.org/2.33/")
             
             testthat::expect_equal(data, "CXw2yu5fodb")
             rm(data)
    }
  )

  test_that(
    paste0("Default behavior, if provide filter property other than name or ", 
           "id then name returned by default: "), {

# httr::content(httr::GET(
#   paste0(
#          "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
#          "paging=false&filter=code:in:[CHC]&fields=code,name")))
             
             data <- getOrgUnitGroups(
               "CHC", by = code,
               base_url = "https://play.dhis2.org/2.33/"
             )
             
             testthat::expect_equal(NROW(data), 1)
             testthat::expect_equal(data, "CHC")
             rm(data)
           }
  )
  
  test_that(
    paste0("If provide filter property other than name or ", 
           "id then can get back other fields: "), {
             
 #httr::content(httr::GET(
 #  paste0(
 #         "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
 #         "paging=false&filter=code:in:[Country,CHC]&fields=code,id,name")))
             
             data <- getOrgUnitGroups(c("Country", "CHC"),
                                      by = code, 
                                      fields = "id",
                                      base_url = "https://play.dhis2.org/2.33/"
             )
             testthat::expect_equal(data, c("RpbiCJpIYEj",
                                            "CXw2yu5fodb"))
             rm(data)

 #httr::content(httr::GET(
 #  paste0(
 #    "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
 #    "paging=false&filter=shortName:in:[Country,CHC]",
 #    "&fields=code,id,name,shortName")))

             data <- getOrgUnitGroups(
               c("Country", "CHC", "Country"),
               by = shortName,
               fields = "code, id, name, shortName",
               base_url = "https://play.dhis2.org/2.33/"
             )
             testthat::expect_equal(NROW(data), 3)
             testthat::expect_named(data, c("code",
                                            "id",
                                            "name",
                                            "shortName"))
             testthat::expect_identical(data$id,
                                        c("RpbiCJpIYEj",
                                          "CXw2yu5fodb",
                                          "RpbiCJpIYEj"))
             rm(data)
             
 #httr::content(httr::GET(
 # paste0(
 #  "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
 #   "paging=false&filter=shortName:in:[CHC,Country]&fields=shortName,id,code")))

             data <- getOrgUnitGroups(
               c("CHC", "Country"), 
               by = shortName, 
               fields = "id, code",
               base_url = "https://play.dhis2.org/2.33/"
             )             
             testthat::expect_equal(NROW(data), 2)
             testthat::expect_named(data, c("id",
                                            "code"))
             testthat::expect_identical(data$id,
                                        c("CXw2yu5fodb",
                                          "RpbiCJpIYEj"))
             rm(data)
           }
  )
  
  test_that(
    paste0("Provide vector of unique IDs and get back ordered",
           "character vector of names based on input order"), {
             
# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
#   "paging=false&filter=id:in:[w1Atoz18PCL,CXw2yu5fodb]",
#   "&fields=name")))
# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
#   "paging=false&filter=id:in:[w1Atoz18PCL,CXw2yu5fodb]",
#   "&fields=id,name")))
             
             data <- getOrgUnitGroups(
               c("w1Atoz18PCL","CXw2yu5fodb"),
               base_url = "https://play.dhis2.org/2.33/"
             )
             testthat::expect_identical(data, c("District","CHC"))
             rm(data)
           })
  
  test_that(
    paste0("Provide vector of non-unique IDs and get back ordered",
           "character vector of names based on input order"), {
             
# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
#   "paging=false&filter=id:in:[w1Atoz18PCL,CXw2yu5fodb]",
#   "&fields=name,id")))
             
      data <- getOrgUnitGroups(
        c("w1Atoz18PCL","CXw2yu5fodb", 
          "w1Atoz18PCL","w1Atoz18PCL",
          "CXw2yu5fodb","CXw2yu5fodb"),
        base_url = "https://play.dhis2.org/2.33/"
      )
      testthat::expect_identical(data, c("District","CHC",
                                         "District","District",
                                         "CHC","CHC"))
      rm(data)
    }
  )
  
  test_that(
    paste0("Provide vector of non-repeating names and get back ordered",
           "character vector of ids: "),{

# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
#   "paging=false&filter=name:in:[District,CHC]&fields=id,name")))
# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
#   "paging=false&filter=name:in:[District,CHC]&fields=id")))
             
      data <- getOrgUnitGroups(
        c("District","CHC"), by = name,
        base_url = "https://play.dhis2.org/2.33/"
      )
      testthat::expect_identical(data,  c("w1Atoz18PCL","CXw2yu5fodb"))
      rm(data)
    }
  )

  test_that("Uses default base_url: ", {
    original_baseurl <- getOption("baseurl")
    options("baseurl" = "https://play.dhis2.org/2.33/")
    data <- getOrgUnitGroups("CXw2yu5fodb")
    testthat::expect_equal(data, "CHC")
    options("baseurl" = original_baseurl)
    rm(data)
  })

  test_that("Can specify non-default fields", {
        
# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
#   "paging=false&filter=name:in:[CHP,Rural]",
#   "&fields=name,id,code")))

      data <-
        getOrgUnitGroups(
          c("CHP", "Rural"),
          by = "name",
          fields = c("id", "code"),
          base_url = "https://play.dhis2.org/2.33/"
        )
      testthat::expect_equal(NROW(data), 2)
      testthat::expect_named(data, c("id", "code"))
      testthat::expect_true(is.na(data[[2, 2]]))
      rm(data)
    }
  )

  test_that("Get collections as lists", {
    
# httr::content(httr::GET(paste0(
#   "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
#   "paging=false&filter=name:in:[CHP,Rural]",
#   "&fields=name,id,organisationUnits[name,id],groupSets[name,id]")))
    
      data <-
        getOrgUnitGroups(
          c("CHP", "Rural"),
          by = "name",
          fields = c(
            "name", "id", "organisationUnits[name,id]",
            "groupSets[name,id]"
          ),
          base_url = "https://play.dhis2.org/2.33/"
        )

      testthat::expect_s3_class(data, "data.frame")
      testthat::expect_equal(NROW(data), 2)
      testthat::expect_named(data, c(
        "name", "id",
        "organisationUnits", "groupSets"
      ))
      testthat::expect_equal(
        NROW(tidyr::unnest(data,
          organisationUnits,
          names_sep = "_"
        )),
        655
      )
      rm(data)
    }
  )

  test_that(
    paste0("getOrgUnitGroups can handle repeated values and sorting based on input",
           "with multiple fields"), {

      groups <- rep(c(
        "gzcv65VyaGq", "uYxK4wmcPqA", "RXL3lPSK8oG",
        "RpbiCJpIYEj", "w1Atoz18PCL", "CXw2yu5fodb"
      ), 19)

# randomize order of uids
      rows <- sample(length(groups))
      groups <- c("gzcv65VyaGq", "uYxK4wmcPqA", "RXL3lPSK8oG",
                  "RpbiCJpIYEj", "w1Atoz18PCL", "CXw2yu5fodb",
                  groups[rows])

# httr::GET(paste0("https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
#        "paging=false&filter=id:in:[gzcv65VyaGq,uYxK4wmcPqA,",
#        "RXL3lPSK8oG,RpbiCJpIYEj,w1Atoz18PCL,CXw2yu5fodb]",
#        "&fields=code,name,id"))
      data <-
        getOrgUnitGroups(
          groups,
          fields = "code,name,id",
          base_url = "https://play.dhis2.org/2.33/"
        )

      testthat::expect_equal(NROW(data), 120)
      testthat::expect_identical(groups, data$id)
      rm(data)
    }
  )
})

test_that(
  paste0("getOrgUnitGroups returns error for unsupported",
         "by field"), {
           expect_error(
             getOrgUnitGroups("foo", by = bar)
           )
         })

#test for duplicateResponse function
test_that(
  paste0("duplicateResponse works on dataframes"), {
      resp <- data.frame("a" = c(1,2,3), "b" = c("a","b","c"),
                         stringsAsFactors = F)
      expand <- c("a", "a", "a", "b", "c", "c")
      resp <- duplicateResponse(resp = resp, expand = expand, by = "b")
      testthat::expect_equal(NROW(resp), 6)
      testthat::expect_identical(expand, resp$b)
      rm(resp)
         })

#test for simplify structure function
test_that(
  paste0("simplifySttructure works on nested dataframes contained in lists"), {
      resp <- data.frame("a" = c(1,2,3), "b" = c("a","b","c"),
                         stringsAsFactors = F)
      resp <- data.frame("a" = data.frame("a" = "b"), "b" = list(resp))
      resp <- list(resp)
      resp <- simplifyStructure(resp)
      testthat::expect_s3_class(resp, "data.frame")
      rm(resp)
         })




