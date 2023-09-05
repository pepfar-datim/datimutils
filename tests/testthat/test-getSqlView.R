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

data <- listSqlViews(d2_session = play2.37.10)

testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 10)
testthat::expect_named(data, c("id", "displayName"), ignore.order = TRUE)
testthat::expect_error(listSqlViews("foo"))
rm(data) })

test_that("getSqlView: ", {
#httr::content(httr::GET(paste0(
#"https://play.dhis2.org/2.35.6/api/sqlViews/tw3A6ZXOdbA/data.json?",
#"paging=false&var=valuetype:TEXT&filter=valuetype:ilike:TEXT")))

data <- getSqlView(sql_view_uid = "qMYMT0iUGkG",
                  variable_keys = c("valueType"),
                  variable_values = c("TEXT"),
                  d2_session = play2.37.10)

testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 161)
rm(data) })

test_that("getSqlView verbose: ", {
  data2 <- getSqlView(sql_view_uid = "tw3A6ZXOdbA",
                      valuetype %.like% "TEXT",
                      d2_session = play2.37.10,
                      verbose = TRUE)

  testthat::expect_type(data2, "list")
  testthat::expect_s3_class(data2$data, "data.frame")
  testthat::expect_equal(NROW(data2$data), 174)
  rm(data2) })

# https://play.dhis2.org/2.35.13/api/sqlViews/QAlOivHBY3a/data.json?paging=false
test_that("We can fetch a SQL view without nested lists", {
  test_dataset <-  getSqlView(sql_view_uid = "QAlOivHBY3a",
                              d2_session = play2.37.10)

  testthat::expect_s3_class(test_dataset, "data.frame")
  test_dataset_names <-  c("categoryoptioncomboid",
                           "categoryoptioncomboname",
                           "approvallevel",
                           "startdate",
                           "enddate",
                           "uid")
  testthat::expect_true(setequal(test_dataset_names, names(test_dataset)))

})


# https://play.dhis2.org/2.35.13/api/sqlViews/QAlOivHBY3a/data.json?paging=false&filter=categoryoptioncomboid:eq:1
test_that("We can fetch a SQL view with zero rows", {
  test_dataset <-  getSqlView("categoryoptioncomboid:eq:1",
                              sql_view_uid = "QAlOivHBY3a",
                              d2_session = play2.37.10)

  testthat::expect_null(test_dataset, "data.frame")
  testthat::expect_equal(NROW(test_dataset), 0)
})

test_that("getSqlView: add", {

  data <- getSqlView(sql_view_uid = "tw3A6ZXOdbA",
                     d2_session = play2.37.10
  )

  testthat::expect_s3_class(data, "data.frame")
  testthat::expect_equal(NROW(data), 1039)
  rm(data) })

})
