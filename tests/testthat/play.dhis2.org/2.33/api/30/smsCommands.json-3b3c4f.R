structure(list(
  url = "https://play.dhis2.org/2.33.3/api/30/smsCommands.json?paging=false",
  status_code = 200L, headers = structure(list(
    server = "nginx/1.17.9",
    date = "Fri, 24 Apr 2020 12:11:39 GMT", `content-type` = "application/json;charset=UTF-8",
    `transfer-encoding` = "chunked", connection = "keep-alive",
    `cache-control` = "no-cache, private", `x-xss-protection` = "1; mode=block",
    `x-frame-options` = "SAMEORIGIN", `x-content-type-options` = "nosniff",
    etag = "W/\"032cd06e17397c081d9b0420e62841e20\"", `strict-transport-security` = "max-age=15768000",
    `content-encoding` = "gzip"
  ), class = c(
    "insensitive",
    "list"
  )), all_headers = list(list(
    status = 302L, version = "HTTP/1.1",
    headers = structure(list(
      server = "nginx/1.17.9", date = "Fri, 24 Apr 2020 12:11:39 GMT",
      `content-type` = "text/html", `content-length` = "145",
      connection = "keep-alive", location = "https://play.dhis2.org/2.33.3/api/30/smsCommands.json?paging=false",
      `strict-transport-security` = "max-age=15768000"
    ), class = c(
      "insensitive",
      "list"
    ))
  ), list(
    status = 200L, version = "HTTP/1.1",
    headers = structure(list(
      server = "nginx/1.17.9", date = "Fri, 24 Apr 2020 12:11:39 GMT",
      `content-type` = "application/json;charset=UTF-8",
      `transfer-encoding` = "chunked", connection = "keep-alive",
      `cache-control` = "no-cache, private", `x-xss-protection` = "1; mode=block",
      `x-frame-options` = "SAMEORIGIN", `x-content-type-options` = "nosniff",
      etag = "W/\"032cd06e17397c081d9b0420e62841e20\"",
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
  ), class = "data.frame"), content = charToRaw("{\"smsCommands\":[{\"id\":\"nHaOiIJ44VY\",\"displayName\":\"Mortality\"}]}"),
  date = structure(1587730299, class = c("POSIXct", "POSIXt"), tzone = "GMT"), times = c(
    redirect = 0.134959, namelookup = 4.6e-05,
    connect = 4.8e-05, pretransfer = 0.000111, starttransfer = 0.047626,
    total = 0.182636
  )
), class = "response")
