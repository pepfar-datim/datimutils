#library(httptest)
#httptest::start_capturing(simplify = FALSE)
#httptest::stop_capturing()


with_mock_api({

test_that("list all sql views: ", {
#httr::content(httr::GET(
#"https://play.dhis2.org/2.35/api/sqlViews.json?paging=false"))

data <- listSqlViews(base_url = "https://play.dhis2.org/2.35/")

testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 10)
testthat::expect_named(data, c("id", "displayName"))
rm(data) })

  test_that("getSqlView: ", {
#httr::content(httr::GET(
#"https://play.dhis2.org/2.35/api/sqlViews/tw3A6ZXOdbA/data.json?paging=false&var=valuetype:TEXT&filter=valuetype:ilike:TEXT"))

data <- getSqlView(sql_view_uid = "tw3A6ZXOdbA", variable_keys = c("valuetype"), variable_values = c("TEXT"),
                  valuetype %.like% "TEXT",
           base_url = "https://play.dhis2.org/2.35/")

testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 172)
rm(data) })

})