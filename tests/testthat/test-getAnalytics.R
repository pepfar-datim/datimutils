#library(httptest)
#httptest::start_capturing(simplify = FALSE)
#httptest::stop_capturing()

with_mock_api({

test_that("all arguments in getAnalytics function: ", {
#httr::content(httr::GET(
#"https://play.dhis2.org/2.33.5/api/analytics.json?paging=false&dimension=Data:fbfJHSPpUQD;cYeuwXTCPkU&dimension=pe:THIS_YEAR;LAST_YEAR&dimension=ou:ImspTQPwCqd;LEVEL-4&displayProperty=NAME&hierarchyMeta=true&outputIdScheme=UID"))

data <- getAnalytics("displayProperty=NAME", "hierarchyMeta=true", dx = c("fbfJHSPpUQD","cYeuwXTCPkU"),
                     pe = c("THIS_YEAR","LAST_YEAR"), ou = c("ImspTQPwCqd","LEVEL-4"), base_url = "https://play.dhis2.org/2.33.5/")

testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 4394)
testthat::expect_named(data, c("Data","Period","Organisation unit","Value"))
rm(data) })
test_that("Basic eq and sinlge element in call: ", {
#httr::content(httr::GET(
#"https://play.dhis2.org/2.33.5/api/analytics.json?paging=false&dimension=Data:fbfJHSPpUQD.pq2XI5kz2BY;fbfJHSPpUQD.PT59n8BQbqM;cYeuwXTCPkU.pq2XI5kz2BY;cYeuwXTCPkU.PT59n8BQbqM;o15CyZiTvxa;f27B1G7B3m3;hJNC4Bu2Mkv&dimension=pe:201908;201909;201910;201911;201912;202001;202002;202003;202004;202005;202006;202007&filter=ou:ImspTQPwCqd&displayProperty=NAME&outputIdScheme=NAME"))

  data <- getAnalytics("displayProperty=NAME",
                     dx = c("fbfJHSPpUQD.pq2XI5kz2BY","fbfJHSPpUQD.PT59n8BQbqM","cYeuwXTCPkU.pq2XI5kz2BY","cYeuwXTCPkU.PT59n8BQbqM","o15CyZiTvxa","f27B1G7B3m3","hJNC4Bu2Mkv"),
                     pe = c("201908","201909","201910","201911","201912","202001","202002","202003","202004","202005","202006","202007"),
                     ou_f = "ImspTQPwCqd", outputIdScheme="NAME", base_url = "https://play.dhis2.org/2.33.5/")

testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 84)
testthat::expect_named(data, c("Data","Period","Value"))
rm(data)
#  httr::content(httr::GET(
#"https://play.dhis2.org/2.33.5/api/analytics.json?paging=false&dimension=dx:fbfJHSPpUQD.pq2XI5kz2BY;fbfJHSPpUQD.PT59n8BQbqM;cYeuwXTCPkU.pq2XI5kz2BY;cYeuwXTCPkU.PT59n8BQbqM;o15CyZiTvxa;f27B1G7B3m3;hJNC4Bu2Mkv&dimension=pe:201908;201909;201910;201911;201912;202001;202002;202003;202004;202005;202006;202007&filter=ou:ImspTQPwCqd&startDate=2017-01-01&displayProperty=NAME&outputIdScheme=NAME"))
data <- getAnalytics("startDate=2017-01-01","displayProperty=NAME",
                     dx = c("fbfJHSPpUQD.pq2XI5kz2BY","fbfJHSPpUQD.PT59n8BQbqM","cYeuwXTCPkU.pq2XI5kz2BY","cYeuwXTCPkU.PT59n8BQbqM","o15CyZiTvxa","f27B1G7B3m3","hJNC4Bu2Mkv"),
                     pe = c("201908","201909","201910","201911","201912","202001","202002","202003","202004","202005","202006","202007"),
                     ou_f = "ImspTQPwCqd", outputIdScheme="NAME", base_url = "https://play.dhis2.org/2.33.5/")
testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 84)
testthat::expect_named(data, c("Data","Period","Value"))
rm(data)
#httr::content(httr::GET(
#  "https://play.dhis2.org/2.33.5/api/analytics.json?paging=false&dimension=dx:fbfJHSPpUQD.pq2XI5kz2BY;fbfJHSPpUQD.PT59n8BQbqM;cYeuwXTCPkU.pq2XI5kz2BY;cYeuwXTCPkU.PT59n8BQbqM;o15CyZiTvxa;f27B1G7B3m3;hJNC4Bu2Mkv&dimension=pe:201908;201909;201910;201911;201912;202001;202002;202003;202004;202005;202006;202007&filter=ou:ImspTQPwCqd&endDate=2019-01-01&displayProperty=NAME&outputIdScheme=NAME"))
  data <- getAnalytics("endDate=2019-01-01","displayProperty=NAME",
                     dx = c("fbfJHSPpUQD.pq2XI5kz2BY","fbfJHSPpUQD.PT59n8BQbqM","cYeuwXTCPkU.pq2XI5kz2BY","cYeuwXTCPkU.PT59n8BQbqM","o15CyZiTvxa","f27B1G7B3m3","hJNC4Bu2Mkv"),
                     pe = c("201908","201909","201910","201911","201912","202001","202002","202003","202004","202005","202006","202007"),
                     ou_f = "ImspTQPwCqd", outputIdScheme="NAME", base_url = "https://play.dhis2.org/2.33.5/")
testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 84)
testthat::expect_named(data, c("Data","Period","Value"))
rm(data)

  #httr::content(httr::GET(
#"https://play.dhis2.org/2.33.5/api/analytics.json?paging=false&dimension=dx:fbfJHSPpUQD.pq2XI5kz2BY;fbfJHSPpUQD.PT59n8BQbqM;cYeuwXTCPkU.pq2XI5kz2BY;cYeuwXTCPkU.PT59n8BQbqM;o15CyZiTvxa;f27B1G7B3m3;hJNC4Bu2Mkv&dimension=pe:201908;201909;201910;201911;201912;202001;202002;202003;202004;202005;202006;202007&filter=ou:ImspTQPwCqd&order=DESC&endDate=2019-01-01&displayProperty=NAME&outputIdScheme=NAME"))
    data <- getAnalytics("order=DESC","endDate=2019-01-01","displayProperty=NAME",
                     dx = c("fbfJHSPpUQD.pq2XI5kz2BY","fbfJHSPpUQD.PT59n8BQbqM","cYeuwXTCPkU.pq2XI5kz2BY","cYeuwXTCPkU.PT59n8BQbqM","o15CyZiTvxa","f27B1G7B3m3","hJNC4Bu2Mkv"),
                     pe = c("201908","201909","201910","201911","201912","202001","202002","202003","202004","202005","202006","202007"),
                     ou_f = "ImspTQPwCqd", outputIdScheme="NAME", base_url = "https://play.dhis2.org/2.33.5/")

testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 84)
testthat::expect_named(data, c("Data","Period","Value"))
testthat::expect_identical(data$Value[1], 362454.0)
#httr::content(httr::GET(
#  "https://play.dhis2.org/2.33.5/api/analytics.json?paging=false&dimension=dx:fbfJHSPpUQD.pq2XI5kz2BY;fbfJHSPpUQD.PT59n8BQbqM;cYeuwXTCPkU.pq2XI5kz2BY;cYeuwXTCPkU.PT59n8BQbqM;o15CyZiTvxa;f27B1G7B3m3;hJNC4Bu2Mkv&dimension=pe:201908;201909;201910;201911;201912;202001;202002;202003;202004;202005;202006;202007&filter=ou:ImspTQPwCqd&tableLayout=true&columns=pe&rows=dx&endDate=2019-01-01&displayProperty=NAME&outputIdScheme=NAME"))
  data <- getAnalytics("tableLayout=true&columns=pe&rows=dx","endDate=2019-01-01","displayProperty=NAME",
                     dx = c("fbfJHSPpUQD.pq2XI5kz2BY","fbfJHSPpUQD.PT59n8BQbqM","cYeuwXTCPkU.pq2XI5kz2BY","cYeuwXTCPkU.PT59n8BQbqM","o15CyZiTvxa","f27B1G7B3m3","hJNC4Bu2Mkv"),
                     pe = c("201908","201909","201910","201911","201912","202001","202002","202003","202004","202005","202006","202007"),
                     ou_f = "ImspTQPwCqd", outputIdScheme="NAME", base_url = "https://play.dhis2.org/2.33.5/")
testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 7)
  testthat::expect_equal(NCOL(data), 16)


})
test_that("multiple filters: ", {
#httr::content(httr::GET(
#"https://play.dhis2.org/2.33.5/api/analytics.json?paging=false&dimension=dx:BfMAe6Itzgt.REPORTING_RATE;QX4ZTUbOt3a.REPORTING_RATE;eZDhcZi6FLP.REPORTING_RATE&dimension=ou:USER_ORGUNIT;USER_ORGUNIT_CHILDREN&filter=pe:LAST_52_WEEKS;LAST_12_MONTHS&displayProperty=NAME&user=xE7jOejl9FI&outputIdScheme=UID"))
    data <- getAnalytics("displayProperty=NAME","user=xE7jOejl9FI",
                     dx = c("BfMAe6Itzgt.REPORTING_RATE","QX4ZTUbOt3a.REPORTING_RATE","eZDhcZi6FLP.REPORTING_RATE"),
                     ou = c("USER_ORGUNIT","USER_ORGUNIT_CHILDREN"),
                     pe_f = c("LAST_52_WEEKS","LAST_12_MONTHS"), base_url = "https://play.dhis2.org/2.33.5/")

testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 42)
testthat::expect_named(data, c("Data","Organisation unit","Value"))
rm(data)
})


})





