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
  test_that(
    paste0("Default behavior, provide id get back name",
           "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
           "paging=false&filter=id:in:[CXw2yu5fodb]&fields=name"
    ), {
      data <- datimutils::getOrgUnitGroups(
        "CXw2yu5fodb",
        base_url = "https://play.dhis2.org/2.33/"
      )
      testthat::expect_equal(data, "CHC")
      rm(data)
    }
  )
  
  test_that(
    paste0("Default behavior, provide name get back id (with standard",
           "evaluation of by): ",
           "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
           "paging=false&filter=id:in:[CHC]&fields=id"
    ),
    {
      data <- datimutils::getOrgUnitGroups(
        "CHC", by = "name",
        base_url = "https://play.dhis2.org/2.33/"
      )
      testthat::expect_equal(data, "CXw2yu5fodb")
      rm(data)
    }
  )
  
  test_that(
    paste0("Default behavior, provide name get back id", 
           "(non standard evaluation of by):",
           "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
           "paging=false&filter=id:in:[CHC]&fields=id"
    ), {
      # with non standard evaluation for by
      data <- datimutils::getOrgUnitGroups(
        "CHC", by = name,
        base_url = "https://play.dhis2.org/2.33/"
      )
      testthat::expect_equal(data, "CXw2yu5fodb")
      rm(data)
    }
  )

  test_that(
    paste0("Default behavior, if provide filter property other than name or", 
           "id then name and id returned by default: ",
           "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
           "paging=false&filter=code:in:[CHC]&fields=name,id"
    ), {
      test_that::expect_error(
        datimutils::getOrgUnitGroups(
          "CHC", by = code,
          base_url = "https://play.dhis2.org/2.33/"
          )
        )
      
      testthat::expect_equal(NROW(data), 1)
      testthat::expect_named(data, c("name", "id"))
      rm(data)
      }
  )
  
  test_that(
    paste0("Provide vector of unique IDs and get back ordered",
           "character vector of names based on input order",
      "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
      "paging=false&filter=id:in:[w1Atoz18PCL,CXw2yu5fodb]"
    ),{
      data <- datimutils::getOrgUnitGroups(
        c("w1Atoz18PCL","CXw2yu5fodb"),
        base_url = "https://play.dhis2.org/2.33/"
      )
      testthat::expect_identical(data, c("District","CHC"))
      rm(data)
      
    }
  )
  
  test_that(
    paste0("Provide vector of non-unique IDs and get back ordered",
           "character vector of names based on input order",
           "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
           "paging=false&filter=id:in:[w1Atoz18PCL,CXw2yu5fodb]"
    ),
    {
      data <- datimutils::getOrgUnitGroups(
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
           "character vector of ids: ",
           "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
           "paging=false&filter=name:in:[District,CHC]"
    ),
    {
      data <- datimutils::getOrgUnitGroups(
        c("District","CHC"), by = name,
        base_url = "https://play.dhis2.org/2.33/"
      )
      testthat::expect_identical(data,  c("w1Atoz18PCL","CXw2yu5fodb"))
      rm(data)
    }
  )

  test_that("Uses default base_url", {
    original_baseurl <- getOption("baseurl")
    options("baseurl" = "https://play.dhis2.org/2.33/")
    data <- datimutils::getOrgUnitGroups("CXw2yu5fodb")
    testthat::expect_equal(data, "CHC")
    options("baseurl" = original_baseurl)
    rm(data)
  })

  test_that(
    paste0(
      "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
      "paging=false&filter=name:in:[CHP,Rural]",
      "&fields=id,code"
    ),
    {
      data <-
        datimutils::getOrgUnitGroups(
          c("CHP", "Rural"),
          by = "name",
          fields = c("id", "code"),
          base_url = "https://play.dhis2.org/2.33/"
        )
      testthat::expect_equal(NROW(data), 2)
      testthat::expect_named(data, c("code", "id"))
      testthat::expect_true(is.na(data[[2, 1]]))
      rm(data)
    }
  )

  test_that(
    paste0(
      "https://play.dhis2.org/2.33/api/organisationUnitGroups.json?",
      "paging=false&filter=name:in:[CHP,Rural]",
      "&fields=name,id,organisationUnits[name,id],groupSets[name,id]"
    ),
    {
      data <-
        datimutils::getOrgUnitGroups(
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
        "name", "id", "groupSets",
        "organisationUnits"
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
    paste0("getOrgUnitGroups can handle repeated values and sorting based on input"),
    {

      # play.dhis2.org will return results in a different order than the order in groups
      groups <- rep(c(
        "gzcv65VyaGq", "uYxK4wmcPqA", "RXL3lPSK8oG",
        "RpbiCJpIYEj", "w1Atoz18PCL", "CXw2yu5fodb"
      ))
      data <-
        datimutils::getOrgUnitGroups(
          groups,
          fields = "code,name,id",
          base_url = "https://play.dhis2.org/2.33/"
        )
      testthat::expect_identical(groups, data$id)
      rm(data)

      groups <- rep(
        groups,
        300
      )
      data <-
        datimutils::getOrgUnitGroups(
          groups,
          fields = "code,name,id",
          base_url = "https://play.dhis2.org/2.33/"
        )

      testthat::expect_equal(NROW(data), 1800)
      testthat::expect_identical(groups, data$id)
      rm(data)
    }
  )
})
