library(httptest)
# httptest::start_capturing(simplify = FALSE)
# httptest::stop_capturing()

# capture_requests({
#   
# #Test add
# play237=loginToDATIM(
#     base_url = 'play.dhis2.org/2.37/',
#     username = 'admin',
#     password = 'district')
# 
# getSqlView(sql_view_uid = "tw3A6ZXOdbA", 
#                    d2_session = play237)
# 
# })


with_mock_api({

test_that("list all sql views: ", {
#httr::content(httr::GET(
#"https://play.dhis2.org/2.35/api/sqlViews.json?paging=false"))

data <- listSqlViews(d2_session = play2335)

testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 10)
testthat::expect_named(data, c("id", "displayName"))
rm(data) })

test_that("getSqlView: ", {
#httr::content(httr::GET(
#"https://play.dhis2.org/2.35.6/api/sqlViews/tw3A6ZXOdbA/data.json?paging=false&var=valuetype:TEXT&filter=valuetype:ilike:TEXT"))

data <- getSqlView(sql_view_uid = "tw3A6ZXOdbA", 
                  variable_keys = c("valuetype"), 
                  variable_values = c("TEXT"),
                  valuetype %.like% "TEXT",
                  d2_session = play2335)

testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 172)
rm(data) })
  
test_that("getSqlView: add", {
  # httr::content(httr::GET(
  # "play.dhis2.org/2.37/api/sqlViews/tw3A6ZXOdbA/data.json&paging=false"))
  
  play237=loginToDATIM(
      base_url = 'play.dhis2.org/2.37/',
      username = 'admin',
      password = 'district')

  
  data <- getSqlView(sql_view_uid = "tw3A6ZXOdbA")
  
  testthat::expect_s3_class(data, "data.frame")
  #testthat::expect_equal(NROW(data), 172)
  rm(data) })

})