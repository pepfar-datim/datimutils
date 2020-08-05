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

   #httr::content(httr::GET(
   #paste0(
   # "https://play.dhis2.org/2.34/api/organisationUnitGroups.json?",
   #  "paging=false&filter=shortName:in:[CHC,Country]&fields=shortName,:all,name")))
  
  data <- getOrgUnitGroups(
    c("CHC", "Country"),
    by = shortName,
    fields = ":all",
    base_url = "https://play.dhis2.org/2.34/"
  )

    testthat::expect_equal(NROW(data), 2)
    testthat::expect_equal(NCOL(data), 30)
    rm(data)
    })

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
  

      org_units = c("Adonkia CHP", "Afro Arab Clinic")
# httr::content(httr::GET(
#   paste0("https://play.dhis2.org/2.34/api/organisationUnits.json?",
#          "paging=false&filter=name:in:[Adonkia%20CHP,Afro%20Arab%20Clinic]",
#          "&fields=name,organisationUnitGroups[name]")))
      testthat::expect_identical(
        getMetadata(
          organisationUnits,
          name %.in% org_units,
          fields = "name,organisationUnitGroups[name]",
          base_url = 
            "https://play.dhis2.org/2.34/")[["organisationUnitGroups"]],
        getOrgUnits(org_units, 
                    by = name,
                    fields = "organisationUnitGroups[name]",
                    base_url = "https://play.dhis2.org/2.34/")
      )

# httr::content(httr::GET(
#   paste0("https://play.dhis2.org/2.34/api/organisationUnits.json?",
#          "paging=false&filter=name:in:[Adonkia%20CHP,Afro%20Arab%20Clinic]",
#          "&fields=name,organisationUnitGroups[name,id]")))
      testthat::expect_identical(
        getMetadata(
          organisationUnits,
          name %.in% org_units,
          fields = "name,organisationUnitGroups[name,id]",
          base_url = 
            "https://play.dhis2.org/2.34/")[["organisationUnitGroups"]],
        getOrgUnits(org_units, 
                    by = name,
                    fields = "organisationUnitGroups[name,id]",
                    base_url = "https://play.dhis2.org/2.34/")
      )
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

    test_that("Test other metadata helpers", {

#httr::content(httr::GET(paste0(
#"https://play.dhis2.org/2.33.5/api/categories.json?paging=false&filter=id:in:[KfdsGBcoiCa]&fields=id,name")))
#
#httr::content(httr::GET(paste0(
#"https://play.dhis2.org/2.33.5/api/categoryCombos.json?paging=false&filter=id:in:[m2jTvAj5kkm]&fields=id,name")))
#
#      httr::content(httr::GET(paste0(
#"https://play.dhis2.org/2.33.5/api/categoryOptionCombos.json?paging=false&filter=id:in:[sqGRzCziswD]&fields=id,name")))
#
#      httr::content(httr::GET(paste0(
#"https://play.dhis2.org/2.33.5/api/categoryOptionGroupSets.json?paging=false&filter=id:in:[C31vHZqu0qU]&fields=id,name")))
#
#      httr::content(httr::GET(paste0(
#"https://play.dhis2.org/2.33.5/api/categoryOptionGroups.json?paging=false&filter=id:in:[OK2Nr4wdfrZ]&fields=id,name")))
#
#      httr::content(httr::GET(paste0(
#"https://play.dhis2.org/2.33.5/api/categoryOptions.json?paging=false&filter=id:in:[FbLZS3ueWbQ]&fields=id,name")))
#
#      httr::content(httr::GET(paste0(
#"https://play.dhis2.org/2.33.5/api/dataElementGroupSets.json?paging=false&filter=id:in:[jp826jAJHUc]&fields=id,name")))
#
#      httr::content(httr::GET(paste0(
#"https://play.dhis2.org/2.33.5/api/dataElementGroups.json?paging=false&filter=id:in:[oDkJh5Ddh7d]&fields=id,name")))
#
#      httr::content(httr::GET(paste0(
#"https://play.dhis2.org/2.33.5/api/dataElements.json?paging=false&filter=id:in:[FTRrcoaog83]&fields=id,name")))
#
#      httr::content(httr::GET(paste0(
#"https://play.dhis2.org/2.33.5/api/dataSets.json?paging=false&filter=id:in:[lyLU2wR22tC]&fields=id,name")))
#
#      httr::content(httr::GET(paste0(
#"https://play.dhis2.org/2.33.5/api/indicatorGroupSets.json?paging=false&filter=id:in:[tOwnTs7TL3Y]&fields=id,name")))
#
#      httr::content(httr::GET(paste0(
#"https://play.dhis2.org/2.33.5/api/indicatorGroups.json?paging=false&filter=id:in:[oehv9EO3vP7]&fields=id,name")))
#
#      httr::content(httr::GET(paste0(
#"https://play.dhis2.org/2.33.5/api/indicators.json?paging=false&filter=id:in:[ReUHfIn0pTQ]&fields=id,name")))
#
#      httr::content(httr::GET(paste0(
#"https://play.dhis2.org/2.33.5/api/optionGroupSets.json?paging=false")))
#
#      httr::content(httr::GET(paste0(
#"https://play.dhis2.org/2.33.5/api/optionGroups.json?paging=false")))
#
#      httr::content(httr::GET(paste0(
#"https://play.dhis2.org/2.33.5/api/optionSets.json?paging=false&filter=id:in:[VQ2lai3OfVG]&fields=id,name")))
#
#      httr::content(httr::GET(paste0(
#"https://play.dhis2.org/2.33.5/api/options.json?paging=false&filter=id:in:[Y1ILwhy5VDY]&fields=id,name")))
#
#        httr::content(httr::GET(paste0(
#"https://play.dhis2.org/2.33.5/api/organisationUnitGroupSets.json?paging=false&filter=id:in:[uIuxlbV1vRT]&fields=id,name")))
#
#        httr::content(httr::GET(paste0(
#"https://play.dhis2.org/2.33.5/api/organisationUnits.json?paging=false&filter=id:in:[Rp268JB6Ne4]&fields=id,name")))

  #httr::content(httr::GET(paste0(
  #  "https://play.dhis2.org/2.33.5/api/optionGroupSets.json?paging=false&filter=id:in:[Wonln7Yg5Am]&fields=id,name")))
  #
   #  httr::content(httr::GET(paste0(
   # "https://play.dhis2.org/2.33.5/api/optionGroups.json?paging=false&filter=id:in:[hTDovVfKAuN]&fields=id,name")))

      data <- getCategories(
        "KfdsGBcoiCa",
        base_url = "https://play.dhis2.org/2.33.5/")
      testthat::expect_identical(data,"Births attended by")
      rm(data)

      data <- getCatCombos(
        "m2jTvAj5kkm",
        base_url = "https://play.dhis2.org/2.33.5/")
      testthat::expect_identical(data,"Births")
      rm(data)

      data <- getCatOptionCombos(
        "sqGRzCziswD",
        base_url = "https://play.dhis2.org/2.33.5/")
      testthat::expect_identical(data,"0-11m")
      rm(data)

      data <- getCatOptionGroupSets(
        "C31vHZqu0qU",
        base_url = "https://play.dhis2.org/2.33.5/")
      testthat::expect_identical(data,"Donor")
      rm(data)

      data <- getCatOptionGroups(
        "OK2Nr4wdfrZ",
        base_url = "https://play.dhis2.org/2.33.5/")
      testthat::expect_identical(data,"CDC")
      rm(data)

      data <- getCatOptions(
        "FbLZS3ueWbQ",
        base_url = "https://play.dhis2.org/2.33.5/")
      testthat::expect_identical(data,"0-11m")
      rm(data)

      data <- getDataElementGroupSets(
        "jp826jAJHUc",
        base_url = "https://play.dhis2.org/2.33.5/")
      testthat::expect_identical(data,"Diagnosis")
      rm(data)

      data <- getDataElementGroups(
        "oDkJh5Ddh7d",
        base_url = "https://play.dhis2.org/2.33.5/")
      testthat::expect_identical(data,"Acute Flaccid Paralysis (AFP) ")
      rm(data)

      data <- getDataElements(
        "FTRrcoaog83",
        base_url = "https://play.dhis2.org/2.33.5/")
      testthat::expect_identical(data,"Accute Flaccid Paralysis (Deaths < 5 yrs)")
      rm(data)

      data <- getDataSets(
        "lyLU2wR22tC",
        base_url = "https://play.dhis2.org/2.33.5/")
      testthat::expect_identical(data,"ART monthly summary")
      rm(data)

      data <- getIndicatorGroupSets(
        "tOwnTs7TL3Y",
        base_url = "https://play.dhis2.org/2.33.5/")
      testthat::expect_identical(data,"Child health")
      rm(data)

      data <- getIndicatorGroups(
        "oehv9EO3vP7",
        base_url = "https://play.dhis2.org/2.33.5/")
      testthat::expect_identical(data,"ANC")
      rm(data)

      data <- getIndicators(
        "ReUHfIn0pTQ",
        base_url = "https://play.dhis2.org/2.33.5/")
      testthat::expect_identical(data,"ANC 1-3 Dropout Rate")
      rm(data)

      data <- getOptionGroupSets(
        "Wonln7Yg5Am",
        base_url = "https://play.dhis2.org/2.33.5/")
      testthat::expect_identical(data,"test")
      rm(data)

      data <- getOptionGroups(
        "hTDovVfKAuN",
        base_url = "https://play.dhis2.org/2.33.5/")
      testthat::expect_identical(data,NULL)
      rm(data)

      data <- getOptionSets(
        "VQ2lai3OfVG",
        base_url = "https://play.dhis2.org/2.33.5/")
      testthat::expect_identical(data,"Age category")
      rm(data)

      data <- getOptions(
        "Y1ILwhy5VDY",
        base_url = "https://play.dhis2.org/2.33.5/")
      testthat::expect_identical(data,"0-14 years")
      rm(data)

      data <- getOrgUnitGroupSets(
        "uIuxlbV1vRT",
        base_url = "https://play.dhis2.org/2.33.5/")
      testthat::expect_identical(data,"Area")
      rm(data)

      data <- getOrgUnits(
        "Rp268JB6Ne4",
        base_url = "https://play.dhis2.org/2.33.5/")
      testthat::expect_identical(data,"Adonkia CHP")
      rm(data)

      })

  test_that("check getorgunitgroups on datim api", {
    data <- getOrgUnitGroups("Country", by = name,
      fields = "organisationUnits[id,name,level,ancestors[id,name]]",
                             base_url = "https://play.dhis2.org/2.33.5/")
  testthat::expect_equal(NROW(data), 1)
  rm(data)
 #httr::content(httr::GET(paste0(
  #"https://play.dhis2.org/2.33.5/api/organisationUnitGroups.json?paging=false&filter=name:in:[Country]&fields=organisationUnits[id,name,level,ancestors[id,name]]")))
  })
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




