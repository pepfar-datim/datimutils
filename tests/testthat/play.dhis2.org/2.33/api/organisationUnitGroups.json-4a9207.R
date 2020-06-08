structure(list(
  url = "https://play.dhis2.org/2.33.3/api/organisationUnitGroups.json?paging=false&filter=id:in:[CXw2yu5fodb,gzcv65VyaGq]&filter=groupSets.id:in:[J5jldMd8OHv]&fields=code",
  status_code = 200L, headers = structure(list(
    server = "nginx/1.17.9",
    date = "Wed, 20 May 2020 12:55:01 GMT", `content-type` = "application/json;charset=UTF-8",
    `transfer-encoding` = "chunked", connection = "keep-alive",
    `cache-control` = "no-cache, private", `x-xss-protection` = "1; mode=block",
    `x-frame-options` = "SAMEORIGIN", `x-content-type-options` = "nosniff",
    etag = "W/\"052a28a7169731ffc19d67064834f7a54\"", `strict-transport-security` = "max-age=15768000",
    `content-encoding` = "gzip"
  ), class = c(
    "insensitive",
    "list"
  )), all_headers = list(list(
    status = 302L, version = "HTTP/1.1",
    headers = structure(list(
      server = "nginx/1.17.9", date = "Wed, 20 May 2020 12:55:01 GMT",
      `content-type` = "text/html", `content-length` = "145",
      connection = "keep-alive", location = "https://play.dhis2.org/2.33.3/api/organisationUnitGroups.json?paging=false&filter=id:in:[CXw2yu5fodb,gzcv65VyaGq]&filter=groupSets.id:in:[J5jldMd8OHv]&fields=code",
      `strict-transport-security` = "max-age=15768000"
    ), class = c(
      "insensitive",
      "list"
    ))
  ), list(
    status = 200L, version = "HTTP/1.1",
    headers = structure(list(
      server = "nginx/1.17.9", date = "Wed, 20 May 2020 12:55:01 GMT",
      `content-type` = "application/json;charset=UTF-8",
      `transfer-encoding` = "chunked", connection = "keep-alive",
      `cache-control` = "no-cache, private", `x-xss-protection` = "1; mode=block",
      `x-frame-options` = "SAMEORIGIN", `x-content-type-options` = "nosniff",
      etag = "W/\"052a28a7169731ffc19d67064834f7a54\"",
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
  ), class = "data.frame"), content = charToRaw("{\"organisationUnitGroups\":[{\"code\":\"CHC\"}]}"),
  date = structure(1589979301, class = c("POSIXct", "POSIXt"), tzone = "GMT"), times = c(
    redirect = 0.04099, namelookup = 2.5e-05,
    connect = 2.7e-05, pretransfer = 6.2e-05, starttransfer = 0.046882,
    total = 0.08793
  )
), class = "response")
