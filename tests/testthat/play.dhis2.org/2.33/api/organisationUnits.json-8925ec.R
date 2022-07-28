structure(list(
  url = "https://play.dhis2.org/2.33.4/api/organisationUnits.json?paging=false&filter=name:like:Sierra%20Leone",
  status_code = 200L, headers = structure(list(
    server = "nginx/1.17.9",
    date = "Fri, 05 Jun 2020 17:43:54 GMT", `content-type` = "application/json;charset=UTF-8",
    `transfer-encoding` = "chunked", connection = "keep-alive",
    `cache-control` = "no-cache, private", `x-xss-protection` = "1; mode=block",
    `x-frame-options` = "SAMEORIGIN", `x-content-type-options` = "nosniff",
    etag = "W/\"07b7271a7178e40f63ac285344a89b423\"", `strict-transport-security` = "max-age=15768000",
    `content-encoding` = "gzip"
  ), class = c(
    "insensitive",
    "list"
  )), all_headers = list(list(
    status = 302L, version = "HTTP/1.1",
    headers = structure(list(
      server = "nginx/1.17.9", date = "Fri, 05 Jun 2020 17:43:54 GMT",
      `content-type` = "text/html", `content-length` = "145",
      connection = "keep-alive", location = "https://play.dhis2.org/2.33.4/api/organisationUnits.json?paging=false&filter=name:like:Sierra%20Leone",
      `strict-transport-security` = "max-age=15768000"
    ), class = c(
      "insensitive",
      "list"
    ))
  ), list(
    status = 200L, version = "HTTP/1.1",
    headers = structure(list(
      server = "nginx/1.17.9", date = "Fri, 05 Jun 2020 17:43:54 GMT",
      `content-type` = "application/json;charset=UTF-8",
      `transfer-encoding` = "chunked", connection = "keep-alive",
      `cache-control` = "no-cache, private", `x-xss-protection` = "1; mode=block",
      `x-frame-options` = "SAMEORIGIN", `x-content-type-options` = "nosniff",
      etag = "W/\"07b7271a7178e40f63ac285344a89b423\"",
      `strict-transport-security` = "max-age=15768000",
      `content-encoding` = "gzip"
    ), class = c(
      "insensitive",
      "list"
    ))
  )), cookies = structure(list(
    domain = "#HttpOnly_play.dhis2.org",
    flag = FALSE, path = "/2.33.4", secure = TRUE, expiration = structure(Inf, class = c(
      "POSIXct",
      "POSIXt"
    )), name = "JSESSIONID", value = "REDACTED"
  ), row.names = c(
    NA,
    -1L
  ), class = "data.frame"), content = charToRaw("{\"organisationUnits\":[{\"id\":\"ImspTQPwCqd\",\"displayName\":\"Sierra Leone\"}]}"),
  date = structure(1591379034, class = c("POSIXct", "POSIXt"), tzone = "GMT"), times = c(
    redirect = 0.045301, namelookup = 4.7e-05,
    connect = 4.9e-05, pretransfer = 0.000119, starttransfer = 0.0532,
    total = 0.098535
  )
), class = "response")
