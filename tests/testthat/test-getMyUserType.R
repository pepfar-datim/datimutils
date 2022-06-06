# testing getMyUserType

# STEPS
# 1. mock apis are recorded in the "test" server
# 2. we loop through user_group_list a list object in helper.R with all response params and checks
# 3. test base url and handle are passed from helper.R to this custom d2 session
# 3. we test the response against th expected streams for that user in helper.R

# accounts ----------

# list out uids
user_group_list <-
  list(
    uids =
      list(
        c("M9Uer9SioL7", "zpgv1M2Li1Q", "seh1e61fwp1", "XgctRYBpSiR", "TRBfaInIiOK"),
        c("M9Uer9SioL7", "seh1e61fwp1", "G1O0MJgw8rs", "XgctRYBpSiR", "TRBfaInIiOK"),
        c("seh1e61fwp1", "c6hGi8GEZot", "M9Uer9SioL7", "gh9tn4QBbKZ", "CwFniyubXbx", "OoiLAfMTyMx",
          "iuD8wUFz95X", "ik8m9tx6QEw", "TRBfaInIiOK", "SBtczqnORYA"
        ),
        c("o8ap0XE01bh", "c6hGi8GEZot", "zY2t7de7Jzz", "TRBfaInIiOK"),
        c("OoiLAfMTyMx", "cveLo35sHE9"),
        c("BQCE8Nh9TRn", "M9Uer9SioL7", "seh1e61fwp1", "XgctRYBpSiR", "TRBfaInIiOK", "LLXM2rpL69u"
        ),
        c("d3E1aOVZZkZ", "M9Uer9SioL7", "e1HnTm7vg38", "tTSFg9jRcIB", "seh1e61fwp1",
          "TRBfaInIiOK", "zZ711zXQxw8", "c6hGi8GEZot"
        ),
        c("zZ711zXQxw8")
      ),
    streams =
      list(
        c("ER", "ESOP", "HRH"),
        c("ER", "ESOP", "HRH"),
        c("ER", "ESOP", "HRH", "MER", "MOH", "SaSR", "SIMS"),
        c("ESOP", "MER"),
        c("MOH"),
        c("ER", "ESOP", "HRH"),
        c("DHI", "ER", "ESOP", "HRH", "MCAE", "MER"),
        c("DHI")
      ),
    user_type =
      list(
        "Agency",
        "Global Agency",
        "Global",
        "Interagency",
        "MOH",
        "Partner",
        "Global Partner",
        "Unclassified User"
      )
  )


# test proper user classification ----
httptest::with_mock_api({
  test_that("test that user types returned are accurate for specific accounts...",
            {
              lapply(1:length(user_group_list$uids), function(user) {
                # parameters
                uids <- unlist(user_group_list$uids[user])
                user_type <- unlist(user_group_list$user_type[user])
                
                # pull data
                data <-
                  getMyUserType(d2_session = list(
                    base_url = test$base_url,
                    handle = test$handle,
                    me = list(userGroups =
                                as.data.frame(
                                  list(
                                    id = uids
                                    ),
                                  stringsAsFactors = F
                                  )
                              )
                  ))

                # compare data to existing
                testthat::expect_equal(data, user_type)
                rm(data, user_type)
              })
            })
})

# test error handling -----
 httptest::with_mock_api({
   test_that("testing error...", {
     # expect error with invalid ids
     expect_error(
       getMyUserType(
         d2_session = list(
           base_url = test$base_url,
           handle = test$handle,
           me = list(userGroups =
                       as.data.frame(list(id = c(
                         "0001"
                       ))))
         )
       ),
       "There was an error retrieving the user group information! Make sure you are logged into DATIM."
     )
   })
 })
