structure(list(
  url = "https://play.dhis2.org/2.33.4/api/dataElements.json?paging=false&filter=id:eq:FTRrcoaog83",
  status_code = 200L, headers = structure(list(
    server = "nginx/1.17.9",
    date = "Thu, 04 Jun 2020 08:19:01 GMT", `content-type` = "application/json;charset=UTF-8",
    `transfer-encoding` = "chunked", connection = "keep-alive",
    `cache-control` = "no-cache, private", `x-xss-protection` = "1; mode=block",
    `x-frame-options` = "SAMEORIGIN", `x-content-type-options` = "nosniff",
    etag = "W/\"0cf75dcd161af9bd16ec71582ba66a6fa\"", `strict-transport-security` = "max-age=15768000",
    `content-encoding` = "gzip"
  ), class = c(
    "insensitive",
    "list"
  )), all_headers = list(list(
    status = 302L, version = "HTTP/1.1",
    headers = structure(list(
      server = "nginx/1.17.9", date = "Thu, 04 Jun 2020 08:19:01 GMT",
      `content-type` = "text/html", `content-length` = "145",
      connection = "keep-alive", location = "https://play.dhis2.org/2.33.4/api/dataElements.json?paging=false&filter=id:eq:FTRrcoaog83",
      `strict-transport-security` = "max-age=15768000"
    ), class = c(
      "insensitive",
      "list"
    ))
  ), list(
    status = 200L, version = "HTTP/1.1",
    headers = structure(list(
      server = "nginx/1.17.9", date = "Thu, 04 Jun 2020 08:19:01 GMT",
      `content-type` = "application/json;charset=UTF-8",
      `transfer-encoding` = "chunked", connection = "keep-alive",
      `cache-control` = "no-cache, private", `x-xss-protection` = "1; mode=block",
      `x-frame-options` = "SAMEORIGIN", `x-content-type-options` = "nosniff",
      etag = "W/\"0cf75dcd161af9bd16ec71582ba66a6fa\"",
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
  ), class = "data.frame"), content = charToRaw("{\"dataElements\":[{\"id\":\"FTRrcoaog83\",\"displayName\":\"Accute Flaccid Paralysis (Deaths < 5 yrs)\"}]}"),
  date = structure(1591258741, class = c("POSIXct", "POSIXt"), tzone = "GMT"), times = c(
    redirect = 0.128482, namelookup = 3.5e-05,
    connect = 3.7e-05, pretransfer = 9.4e-05, starttransfer = 0.046153,
    total = 0.174686
  )
), class = "response")
