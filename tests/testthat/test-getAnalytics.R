#library(httptest)
#httptest::start_capturing(simplify = FALSE)
#httptest::stop_capturing()

with_mock_api({
  test_that("package stops when timeout is too high: ", {
#httr::content(httr::GET(paste0(
    #"https://play.dhis2.org/2.33.5/api/analytics.json?paging=false&dimension=Data:fbfJHSPpUQD;cYeuwXTCPkU",
    #"&dimension=pe:THIS_YEAR;LAST_YEAR&dimension=ou:ImspTQPwCqd;LEVEL-4&displayProperty=NAME",
    #"&hierarchyMeta=true&outputIdScheme=UID")))

    expect_error(getAnalytics("displayProperty=NAME", "hierarchyMeta=true",
                              dx = c("fbfJHSPpUQD", "cYeuwXTCPkU"),
                              pe = c("THIS_YEAR", "LAST_YEAR"),
                              ou = c("ImspTQPwCqd", "LEVEL-4"),
                              d2_session = play40.0.1, timeout = 301),
                 "Timeout must be 5 minutes or less, please change the timeout parameter!", fixed = TRUE)
})

test_that("all arguments in getAnalytics function: ", {
#httr::content(httr::GET(paste0(
#"https://play.dhis2.org/2.33.5/api/analytics.json?paging=false&",
#"dimension=Data:fbfJHSPpUQD;cYeuwXTCPkU&dimension=pe:THIS_YEAR;LAST_YEAR&",
#"dimension=ou:ImspTQPwCqd;LEVEL-4&displayProperty=NAME&hierarchyMeta=true&outputIdScheme=UID")))

data <- getAnalytics("displayProperty=NAME", "hierarchyMeta=true",
                     dx = c("fbfJHSPpUQD", "cYeuwXTCPkU"),
                     pe = c("THIS_YEAR", "LAST_YEAR"),
                     ou = c("ImspTQPwCqd", "LEVEL-4"),
                     d2_session = play40.0.1, timeout = 80)
data2 <- getAnalytics("displayProperty=NAME", "hierarchyMeta=true",
                      dx = c("fbfJHSPpUQD", "cYeuwXTCPkU"),
                      pe = c("THIS_YEAR", "LAST_YEAR"),
                      ou = c("ImspTQPwCqd", "LEVEL-4"),
                      d2_session = play40.0.1, timeout = 80,
                      verbose = TRUE)

testthat::expect_s3_class(data, "data.frame")
  testthat::expect_type(data2, "list")
testthat::expect_equal(NROW(data), 4394)
testthat::expect_named(data, c("Data", "Period", "Organisation unit", "Value"))
rm(data)
})
test_that("other arguments: ", {
#httr::content(httr::GET(paste0(
#"https://play.dhis2.org/2.33.5/api/analytics.json?paging=false&dimension=Data:fbfJHSPpUQD.pq2XI5kz2BY;",
#"fbfJHSPpUQD.PT59n8BQbqM;cYeuwXTCPkU.pq2XI5kz2BY;cYeuwXTCPkU.PT59n8BQbqM;o15CyZiTvxa;f27B1G7B3m3;hJNC4Bu2Mkv&",
#"dimension=pe:201908;201909;201910;201911;201912;202001;202002;202003;202004;202005;202006;202007&",
#"filter=ou:ImspTQPwCqd&displayProperty=NAME&outputIdScheme=NAME")))

  data <- getAnalytics("displayProperty=NAME",
                     dx = c("fbfJHSPpUQD.pq2XI5kz2BY", "fbfJHSPpUQD.PT59n8BQbqM", "cYeuwXTCPkU.pq2XI5kz2BY",
                            "cYeuwXTCPkU.PT59n8BQbqM", "o15CyZiTvxa", "f27B1G7B3m3", "hJNC4Bu2Mkv"),
                     pe = c("202208", "202209", "202210", "202211", "202212", "202301",
                            "202302", "202303", "202304", "202305", "202306", "202307"),
                     ou_f = "ImspTQPwCqd", return_names = TRUE, d2_session = play40.0.1)

testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 84)
testthat::expect_named(data, c("Data", "Period", "Value"))
rm(data)
#  httr::content(httr::GET(paste0(
#"https://play.dhis2.org/2.33.5/api/analytics.json?paging=false&dimension=dx:fbfJHSPpUQD.pq2XI5kz2BY;",
#"fbfJHSPpUQD.PT59n8BQbqM;cYeuwXTCPkU.pq2XI5kz2BY;cYeuwXTCPkU.PT59n8BQbqM;o15CyZiTvxa;f27B1G7B3m3;hJNC4Bu2Mkv&",
#"dimension=pe:201908;201909;201910;201911;201912;202001;202002;202003;202004;202005;202006;202007&",
#"filter=ou:ImspTQPwCqd&startDate=2017-01-01&displayProperty=NAME&outputIdScheme=NAME")))
data <- getAnalytics("startDate=2017-01-01", "displayProperty=NAME",
                     dx = c("fbfJHSPpUQD.pq2XI5kz2BY", "fbfJHSPpUQD.PT59n8BQbqM", "cYeuwXTCPkU.pq2XI5kz2BY",
                            "cYeuwXTCPkU.PT59n8BQbqM", "o15CyZiTvxa", "f27B1G7B3m3", "hJNC4Bu2Mkv"),
                     pe = c("202208", "202209", "202210", "202211", "202212", "202301",
                            "202302", "202303", "202304", "202305", "202306", "202307"),
                     ou_f = "ImspTQPwCqd", return_names = TRUE,  d2_session = play40.0.1)
testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 84)
testthat::expect_named(data, c("Data", "Period", "Value"))
rm(data)
#httr::content(httr::GET(paste0(
#  "https://play.dhis2.org/2.33.5/api/analytics.json?paging=false&",
#  "dimension=dx:fbfJHSPpUQD.pq2XI5kz2BY;fbfJHSPpUQD.PT59n8BQbqM;cYeuwXTCPkU.pq2XI5kz2BY;cYeuwXTCPkU.PT59n8BQbqM;",
#  "o15CyZiTvxa;f27B1G7B3m3;hJNC4Bu2Mkv&dimension=pe:201908;201909;201910;201911;201912;202001;202002;202003;",
#  "202004;202005;202006;202007&filter=ou:ImspTQPwCqd&endDate=2019-01-01&displayProperty=NAME&outputIdScheme=NAME")))
  data <- getAnalytics("endDate=2024-01-01", "displayProperty=NAME",
                     dx = c("fbfJHSPpUQD.pq2XI5kz2BY", "fbfJHSPpUQD.PT59n8BQbqM", "cYeuwXTCPkU.pq2XI5kz2BY",
                            "cYeuwXTCPkU.PT59n8BQbqM", "o15CyZiTvxa", "f27B1G7B3m3", "hJNC4Bu2Mkv"),
                     pe = c("202208", "202209", "202210", "202211", "202212", "202301",
                            "202302", "202303", "202304", "202305", "202306", "202307"),
                     ou_f = "ImspTQPwCqd", return_names = TRUE,  d2_session = play40.0.1)
testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 84)
testthat::expect_named(data, c("Data", "Period", "Value"))
rm(data)

  #httr::content(httr::GET(paste0(
#"https://play.dhis2.org/2.33.5/api/analytics.json?paging=false&",
#"dimension=dx:fbfJHSPpUQD.pq2XI5kz2BY;fbfJHSPpUQD.PT59n8BQbqM;cYeuwXTCPkU.pq2XI5kz2BY;",
#"cYeuwXTCPkU.PT59n8BQbqM;o15CyZiTvxa;f27B1G7B3m3;hJNC4Bu2Mkv&",
#"dimension=pe:201908;201909;201910;201911;201912;202001;202002;202003;202004;202005;202006;202007&",
#"filter=ou:ImspTQPwCqd&order=DESC&endDate=2019-01-01&displayProperty=NAME&outputIdScheme=NAME")))
    data <- getAnalytics("order=DESC", "endDate=2024-01-01", "displayProperty=NAME",
                     dx = c("fbfJHSPpUQD.pq2XI5kz2BY", "fbfJHSPpUQD.PT59n8BQbqM", "cYeuwXTCPkU.pq2XI5kz2BY",
                            "cYeuwXTCPkU.PT59n8BQbqM", "o15CyZiTvxa", "f27B1G7B3m3", "hJNC4Bu2Mkv"),
                     pe = c("202208", "202209", "202210", "202211", "202212", "202301",
                            "202302", "202303", "202304", "202305", "202306", "202307"),
                     ou_f = "ImspTQPwCqd", return_names = TRUE,  d2_session = play40.0.1)

testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 84)
testthat::expect_named(data, c("Data", "Period", "Value"))
testthat::expect_identical(data$Value[1], 362454.0)
#httr::content(httr::GET(paste0(
#  "https://play.dhis2.org/2.33.5/api/analytics.json?paging=false&",
#  "dimension=dx:fbfJHSPpUQD.pq2XI5kz2BY;fbfJHSPpUQD.PT59n8BQbqM;cYeuwXTCPkU.pq2XI5kz2BY;",
#  "cYeuwXTCPkU.PT59n8BQbqM;o15CyZiTvxa;f27B1G7B3m3;hJNC4Bu2Mkv&",
#  "dimension=pe:201908;201909;201910;201911;201912;202001;202002;202003;202004;202005;202006;202007&",
#  "filter=ou:ImspTQPwCqd&tableLayout=true&columns=pe&rows=dx&endDate=2019-01-01&",
#  "displayProperty=NAME&outputIdScheme=NAME")))
  data <- getAnalytics("tableLayout=true&columns=pe&rows=dx", "endDate=2024-01-01", "displayProperty=NAME",
                     dx = c("fbfJHSPpUQD.pq2XI5kz2BY", "fbfJHSPpUQD.PT59n8BQbqM", "cYeuwXTCPkU.pq2XI5kz2BY",
                            "cYeuwXTCPkU.PT59n8BQbqM", "o15CyZiTvxa", "f27B1G7B3m3", "hJNC4Bu2Mkv"),
                     pe = c("202208", "202209", "202210", "202211", "202212", "202301",
                            "202302", "202303", "202304", "202305", "202306", "202307"),
                     ou_f = "ImspTQPwCqd", return_names = TRUE, d2_session = play40.0.1)
testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 7)
  testthat::expect_equal(NCOL(data), 16)


})
test_that("multiple filters: ", {
#httr::content(httr::GET(paste0(
#"https://play.dhis2.org/2.33.5/api/analytics.json?paging=false&",
#"dimension=dx:BfMAe6Itzgt.REPORTING_RATE;QX4ZTUbOt3a.REPORTING_RATE;eZDhcZi6FLP.REPORTING_RATE&",
#"dimension=ou:USER_ORGUNIT;USER_ORGUNIT_CHILDREN&filter=pe:LAST_52_WEEKS;LAST_12_MONTHS&",
#"displayProperty=NAME&user=xE7jOejl9FI&outputIdScheme=UID")))
    data <- getAnalytics("displayProperty=NAME", "user=xE7jOejl9FI",
                     dx = c("BfMAe6Itzgt.REPORTING_RATE", "QX4ZTUbOt3a.REPORTING_RATE", "eZDhcZi6FLP.REPORTING_RATE"),
                     ou = c("USER_ORGUNIT", "USER_ORGUNIT_CHILDREN"),
                     pe_f = c("LAST_52_WEEKS", "LAST_12_MONTHS"),  d2_session = play40.0.1)

testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 42)
testthat::expect_named(data, c("Data", "Organisation unit", "Value"))
rm(data)

      data <- getAnalytics(
                     dx = c("BfMAe6Itzgt.REPORTING_RATE", "QX4ZTUbOt3a.REPORTING_RATE", "eZDhcZi6FLP.REPORTING_RATE"),
                     ou = c("USER_ORGUNIT", "USER_ORGUNIT_CHILDREN"),
                     pe_f = c("LAST_52_WEEKS", "LAST_12_MONTHS"),
                     displayProperty = "NAME", user = "xE7jOejl9FI",
                      d2_session = play40.0.1)

testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 42)
testthat::expect_named(data, c("Data", "Organisation unit", "Value"))
rm(data)



  #httr::content(httr::GET(paste0("https://play.dhis2.org/2.34.1/api/analytics.json?",
  #"paging=false&dimension=dx:fbfJHSPpUQD&startDate=2019-01-01&endDate=2019-01-01&outputIdScheme=UID")))
      data <- getAnalytics("startDate=2019-01-01&endDate=2019-01-01",
                     dx = "fbfJHSPpUQD",
                     d2_session = play40.0.1)
  testthat::expect_identical(data, NULL)
  rm(data)
})

test_that("use of 'all' argument: ", {
 # httr::content(httr::GET(paste0("https://play.dhis2.org/2.34.1/api/analytics.json?",
 # "paging=false&dimension=dx:fbfJHSPpUQD&dimension=pe:LAST_12_MONTHS&dimension=co&",
 # "filter=ou:g8upMTyEZGZ;DiszpKrYNg8;egv5Es0QlQP&displayProperty=NAME&outputIdScheme=UID")))
  data <- getAnalytics("displayProperty=NAME",
                     dx = "fbfJHSPpUQD",
                       co = "all",
                       pe = "LAST_12_MONTHS",
                       ou_f = c("g8upMTyEZGZ", "DiszpKrYNg8", "egv5Es0QlQP"),
                     d2_session = play40.0.1)
  testthat::expect_equal(NROW(data), 28)

  #httr::content(httr::GET("https://play.dhis2.org/2.34.1/api/analytics.json?",
  #"paging=false&dimension=dx:fbfJHSPpUQD;cYeuwXTCPkU&dimension=pe:201601&",
  #"dimension=ou:O6uvpzGd5pu;lc3eMKXaEfw&dimension=co&filter=co&outputIdScheme=UID"))

  data <- getAnalytics(
                     dx = c("fbfJHSPpUQD", "cYeuwXTCPkU"),
                       co = "all",
                       pe = "201601",
                       ou = c("O6uvpzGd5pu", "lc3eMKXaEfw"),
                     d2_session = play40.0.1)

  testthat::expect_identical(data, NULL)
})

})



test_that("%.d% and %.f%, make_dim and make_fil: ", {
  testthat::expect_equal(a %.d% c("123", "456"), "dimension=a:123;456")
  testthat::expect_equal(a %.f% c("123", "456"), "filter=a:123;456")
  testthat::expect_equal(a %.d% "all", "dimension=a")
  testthat::expect_equal(a %.f% "all", "filter=a")
  testthat::expect_equal("a" %.d% c("123", "456"), "dimension=a:123;456")
  testthat::expect_equal("a" %.f% c("123", "456"), "filter=a:123;456")

  testthat::expect_equal(make_dim(a, c("123", "456")), "dimension=a:123;456")
  testthat::expect_equal(make_fil(a, c("123", "456")), "filter=a:123;456")
  testthat::expect_equal(make_dim(a, "all"), "dimension=a")
  testthat::expect_equal(make_fil(a, "all"), "filter=a")
  testthat::expect_equal(make_dim("a", c("123", "456")), "dimension=a:123;456")
  testthat::expect_equal(make_fil("a", c("123", "456")), "filter=a:123;456")
})
