###/api/33/analytics?dimension=dx:fbfJHSPpUQD;cYeuwXTCPkU&filter=pe:2014Q1;2014Q2
###  &filter=ou:O6uvpzGd5pu;lc3eMKXaEfw
##
#dimensions <- dForm(c("fbfJHSPpUQD","cYeuwXTCPkU"), id = "dx")
#filters <- fForm(c("2014Q1","2014Q2"), c("O6uvpzGd5pu","lc3eMKXaEfw"), id = c("pe","ou"))
#data <- getAnalytics(dimensions = dimensions, filters = filters, base_url = "https://play.dhis2.org/2.33/")
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




