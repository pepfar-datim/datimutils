structure(list(
  url = "https://play.dhis2.org/2.33.4/api/organisationUnits.json?paging=false&filter=organisationUnitGroups.id:eq:RpbiCJpIYEj&fields=id,name,level,ancestors[id,name]",
  status_code = 200L, headers = structure(list(
    server = "nginx/1.17.9",
    date = "Thu, 04 Jun 2020 08:19:02 GMT", `content-type` = "application/json;charset=UTF-8",
    `transfer-encoding` = "chunked", connection = "keep-alive",
    `cache-control` = "no-cache, private", `x-xss-protection` = "1; mode=block",
    `x-frame-options` = "SAMEORIGIN", `x-content-type-options` = "nosniff",
    etag = "W/\"03fd61fb5c08a6f10ed245ad98bd9a6b0\"", `strict-transport-security` = "max-age=15768000",
    `content-encoding` = "gzip"
  ), class = c(
    "insensitive",
    "list"
  )), all_headers = list(list(
    status = 302L, version = "HTTP/1.1",
    headers = structure(list(
      server = "nginx/1.17.9", date = "Thu, 04 Jun 2020 08:19:02 GMT",
      `content-type` = "text/html", `content-length` = "145",
      connection = "keep-alive", location = "https://play.dhis2.org/2.33.4/api/organisationUnits.json?paging=false&filter=organisationUnitGroups.id:eq:RpbiCJpIYEj&fields=id,name,level,ancestors[id,name]",
      `strict-transport-security` = "max-age=15768000"
    ), class = c(
      "insensitive",
      "list"
    ))
  ), list(
    status = 200L, version = "HTTP/1.1",
    headers = structure(list(
      server = "nginx/1.17.9", date = "Thu, 04 Jun 2020 08:19:02 GMT",
      `content-type` = "application/json;charset=UTF-8",
      `transfer-encoding` = "chunked", connection = "keep-alive",
      `cache-control` = "no-cache, private", `x-xss-protection` = "1; mode=block",
      `x-frame-options` = "SAMEORIGIN", `x-content-type-options` = "nosniff",
      etag = "W/\"03fd61fb5c08a6f10ed245ad98bd9a6b0\"",
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
  ), class = "data.frame"), content = charToRaw("{\"organisationUnits\":[{\"level\":1,\"name\":\"Sierra Leone\",\"id\":\"ImspTQPwCqd\",\"ancestors\":[]}]}"),
  date = structure(1591258742, class = c("POSIXct", "POSIXt"), tzone = "GMT"), times = c(
    redirect = 0.034681, namelookup = 4.8e-05,
    connect = 5.1e-05, pretransfer = 0.000131, starttransfer = 0.06371,
    total = 0.09844
  )
), class = "response")
