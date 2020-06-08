structure(list(
  url = "https://play.dhis2.org/2.33.3/api/organisationUnitGroups.json?paging=false&filter=id:in:[CXw2yu5fodb]&fields=name,id",
  status_code = 200L, headers = structure(list(
    server = "nginx/1.17.9",
    date = "Wed, 20 May 2020 11:16:26 GMT", `content-type` = "application/json;charset=UTF-8",
    `transfer-encoding` = "chunked", connection = "keep-alive",
    `cache-control` = "no-cache, private", `x-xss-protection` = "1; mode=block",
    `x-frame-options` = "SAMEORIGIN", `x-content-type-options` = "nosniff",
    etag = "W/\"060a65457a9700e4dba30d5ca5e800341\"", `strict-transport-security` = "max-age=15768000",
    `content-encoding` = "gzip"
  ), class = c(
    "insensitive",
    "list"
  )), all_headers = list(list(
    status = 302L, version = "HTTP/1.1",
    headers = structure(list(
      server = "nginx/1.17.9", date = "Wed, 20 May 2020 11:16:26 GMT",
      `content-type` = "text/html", `content-length` = "145",
      connection = "keep-alive", location = "https://play.dhis2.org/2.33.3/api/organisationUnitGroups.json?paging=false&filter=id:in:[CXw2yu5fodb]&fields=name,id",
      `strict-transport-security` = "max-age=15768000"
    ), class = c(
      "insensitive",
      "list"
    ))
  ), list(
    status = 200L, version = "HTTP/1.1",
    headers = structure(list(
      server = "nginx/1.17.9", date = "Wed, 20 May 2020 11:16:26 GMT",
      `content-type` = "application/json;charset=UTF-8",
      `transfer-encoding` = "chunked", connection = "keep-alive",
      `cache-control` = "no-cache, private", `x-xss-protection` = "1; mode=block",
      `x-frame-options` = "SAMEORIGIN", `x-content-type-options` = "nosniff",
      etag = "W/\"060a65457a9700e4dba30d5ca5e800341\"",
      `strict-transport-security` = "max-age=15768000",
      `content-encoding` = "gzip"
    ), class = c(
      "insensitive",
      "list"
    ))
  )), cookies = structure(list(
    domain = "#HttpOnly_play.dhis2.org",
    flag = FALSE, path = "/2.33.3", secure = TRUE, expiration = structure(Inf, class = c(
      "POSIXct",
      "POSIXt"
    )), name = "JSESSIONID", value = "REDACTED"
  ), row.names = c(
    NA,
    -1L
  ), class = "data.frame"), content = charToRaw("{\"organisationUnitGroups\":[{\"name\":\"CHC\",\"id\":\"CXw2yu5fodb\"}]}"),
  date = structure(1589973386, class = c("POSIXct", "POSIXt"), tzone = "GMT"), times = c(
    redirect = 0.044546, namelookup = 2.9e-05,
    connect = 3e-05, pretransfer = 5.6e-05, starttransfer = 0.050114,
    total = 0.094708
  )
), class = "response")
