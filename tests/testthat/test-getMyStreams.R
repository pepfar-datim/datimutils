# datim user accounts list

user_list <- list(
  username = 
    list(
      "a_cmr_hhscdc",
      "ga_usaid",
      "global_user_tieriii",
      "ia_zaf",
      "moh_zaf",
      "p_cmr_globalhealth",
      "smokeGP"
    ),
  streams = 
    list(
      list("ER","ESOP","HRH"),
      list("ER","ESOP","HRH"),
      list("ER","ESOP","HRH","MER","MOH","SaSR","SIMS"),
      list("ESOP","MER"),
      list("MOH"),
      list("ER","ESOP","HRH"),
      list("DHI","ER","ESOP","HRH","MCAE","MER" )
    ),
  user_type = 
    list(
      "Agency",
      "Global Agency",
      "Global",
      "Interagency",
      "MOH",
      "Partner",
      "Global Partner"
    )
)


with_mock_api({
  
  test_that("test data streams returned are accurate", {
    
    # loop through users to test each account stream response
    for (i in 1:length(user_list[["username"]])) {
      
      # the user
      user <- i
    
      # test stream response
      streams_to_expect <- unlist(user_list[["streams"]][i])
      response <- getMyStreams(d2_session = play2335)
      
      testthat::expect_s3_class(response, "character")
      
    }
  })
})
