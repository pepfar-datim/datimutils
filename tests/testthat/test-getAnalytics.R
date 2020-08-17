#library(httptest)
#httptest::start_capturing(simplify = FALSE)
#httptest::stop_capturing()

with_mock_api({

test_that("Basic eq and sinlge element in call: ", {
#httr::content(httr::GET(
#"https://play.dhis2.org/2.33.5/api/analytics.json?paging=false&dimension=dx:fbfJHSPpUQD;cYeuwXTCPkU&dimension=pe:THIS_YEAR;LAST_YEAR&dimension=ou:ImspTQPwCqd;LEVEL-4&displayProperty=NAME&hierarchyMeta=true&outputIdScheme=UID"))

dimensions <- dForm(c("fbfJHSPpUQD","cYeuwXTCPkU"), c("THIS_YEAR","LAST_YEAR"), c("ImspTQPwCqd","LEVEL-4"), id = c("dx","pe", "ou"))
data <- getAnalytics("displayProperty=NAME", "hierarchyMeta=true", "outputIdScheme=UID", dimensions = dimensions, base_url = "https://play.dhis2.org/2.33.5/")

testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 4394)
testthat::expect_named(data, c("dx","pe","ou","value"))
rm(data) })
test_that("Basic eq and sinlge element in call: ", {
#httr::content(httr::GET(
#"https://play.dhis2.org/2.33.5/api/analytics.json?paging=false&dimension=dx:fbfJHSPpUQD.pq2XI5kz2BY;fbfJHSPpUQD.PT59n8BQbqM;cYeuwXTCPkU.pq2XI5kz2BY;cYeuwXTCPkU.PT59n8BQbqM;o15CyZiTvxa;f27B1G7B3m3;hJNC4Bu2Mkv&dimension=pe:201908;201909;201910;201911;201912;202001;202002;202003;202004;202005;202006;202007&filter=ou:ImspTQPwCqd&displayProperty=NAME&outputIdScheme=NAME"))
dimensions <- dForm(c("fbfJHSPpUQD.pq2XI5kz2BY","fbfJHSPpUQD.PT59n8BQbqM","cYeuwXTCPkU.pq2XI5kz2BY","cYeuwXTCPkU.PT59n8BQbqM","o15CyZiTvxa","f27B1G7B3m3","hJNC4Bu2Mkv"),
c("201908","201909","201910","201911","201912","202001","202002","202003","202004","202005","202006","202007"), id = c("dx","pe"))
filters <- fForm("ImspTQPwCqd", id = "ou")
data <- getAnalytics("displayProperty=NAME","outputIdScheme=NAME", dimensions = dimensions, filters = filters, base_url = "https://play.dhis2.org/2.33.5/")

testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 84)
testthat::expect_named(data, c("dx","pe","value"))
rm(data)
})
test_that("Basic eq and sinlge element in call: ", {
#httr::content(httr::GET(
#"https://play.dhis2.org/2.33.5/api/analytics.json?paging=false&dimension=dx:BfMAe6Itzgt.REPORTING_RATE;QX4ZTUbOt3a.REPORTING_RATE;eZDhcZi6FLP.REPORTING_RATE&dimension=ou:USER_ORGUNIT;USER_ORGUNIT_CHILDREN&filter=pe:LAST_52_WEEKS;LAST_12_MONTHS&displayProperty=NAME&user=xE7jOejl9FI&outputIdScheme=UID"))
dimensions <- dForm(c("BfMAe6Itzgt.REPORTING_RATE","QX4ZTUbOt3a.REPORTING_RATE","eZDhcZi6FLP.REPORTING_RATE"), c("USER_ORGUNIT","USER_ORGUNIT_CHILDREN"), id = c("dx","ou"))
filters <- fForm(c("LAST_52_WEEKS","LAST_12_MONTHS"), id = "pe")
data <- getAnalytics("displayProperty=NAME","user=xE7jOejl9FI","outputIdScheme=UID", dimensions = dimensions, filters = filters, base_url = "https://play.dhis2.org/2.33.5/")

testthat::expect_s3_class(data, "data.frame")
testthat::expect_equal(NROW(data), 42)
testthat::expect_named(data, c("dx","ou","value"))
rm(data)
})
})
#finish this test


#
#
#
###/api/33/analytics?dimension=dx:fbfJHSPpUQD&dimension=pe:2014Q1&dimension=ou:O6uvpzGd5pu
###  &aggregationType=COUNT
#dimensions <- dForm("fbfJHSPpUQD", "2014Q1", "O6uvpzGd5pu",   id = c("dx","pe","ou"))
#getAnalytics("aggregationType=COUNT", dimensions = dimensions, base_url = "https://play.dhis2.org/2.33/")
##
###/api/33/analytics.json?dimension=dx:fbfJHSPpUQD;cYeuwXTCPkU
###  &dimension=ou:ImspTQPwCqd&startDate=2018-01-01&endDate=2018-06-01
#dimensions <- dForm(c("fbfJHSPpUQD","cYeuwXTCPkU"), "ImspTQPwCqd", id = c("dx","ou"))
#getAnalytics(dimensions = dimensions, start_date = "2018-01-01",
#             end_date = "2018-06-01", base_url = "https://play.dhis2.org/2.33/")
##
###/api/33/analytics.html?dimension=dx:fbfJHSPpUQD;cYeuwXTCPkU&dimension=pe:2014Q1;2014Q2
###  &dimension=ou:O6uvpzGd5pu&tableLayout=true&columns=dx;ou&rows=pe
#dimensions <- dForm(c("fbfJHSPpUQD","cYeuwXTCPkU"), c("2014Q1","2014Q2"), "O6uvpzGd5pu", id = c("dx","pe","ou"))
#getAnalytics(dimensions = dimensions, rows = "pe", columns = c("dx","ou"), base_url = "https://play.dhis2.org/2.33/")
#




