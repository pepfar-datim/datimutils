# testing getMyUserType

# STEPS
# 1. mock apis are recorded in the "test" server
# 2. we loop through uid_list a list object in helper.R with all response params and checks
# 3. test base url and handle are passed from helper.R to this custom d2 session
# 3. we test the response against th expected streams for that user in helper.R


# test proper user classification ----
httptest::with_mock_api({
  test_that("test that user types returned are accurate for specific accounts...",
            {
              lapply(1:length(uid_list$uids), function(user) {
                # parameters
                uids <- unlist(uid_list$uids[user])
                user_type <- unlist(uid_list$user_type[user])
                
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
