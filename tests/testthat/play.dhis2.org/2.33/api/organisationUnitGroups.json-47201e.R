structure(list(
  url = "https://play.dhis2.org/2.33.3/api/organisationUnitGroups.json?paging=false&filter=name:in:[CHP,Rural]&fields=id,code",
  status_code = 200L, headers = structure(list(
    server = "nginx/1.17.9",
    date = "Wed, 20 May 2020 11:16:28 GMT", `content-type` = "application/json;charset=UTF-8",
    `transfer-encoding` = "chunked", connection = "keep-alive",
    `cache-control` = "no-cache, private", `x-xss-protection` = "1; mode=block",
    `x-frame-options` = "SAMEORIGIN", `x-content-type-options` = "nosniff",
    etag = "W/\"0677758b252ee5d23494cd16083e51bdf\"", `strict-transport-security` = "max-age=15768000",
    `content-encoding` = "gzip"
  ), class = c(
    "insensitive",
    "list"
  )), all_headers = list(list(
    status = 302L, version = "HTTP/1.1",
    headers = structure(list(
      server = "nginx/1.17.9", date = "Wed, 20 May 2020 11:16:28 GMT",
      `content-type` = "text/html", `content-length` = "145",
      connection = "keep-alive", location = "https://play.dhis2.org/2.33.3/api/organisationUnitGroups.json?paging=false&filter=name:in:[CHP,Rural]&fields=id,code",
      `strict-transport-security` = "max-age=15768000"
    ), class = c(
      "insensitive",
      "list"
    ))
  ), list(
    status = 200L, version = "HTTP/1.1",
    headers = structure(list(
      server = "nginx/1.17.9", date = "Wed, 20 May 2020 11:16:28 GMT",
      `content-type` = "application/json;charset=UTF-8",
      `transfer-encoding` = "chunked", connection = "keep-alive",
      `cache-control` = "no-cache, private", `x-xss-protection` = "1; mode=block",
      `x-frame-options` = "SAMEORIGIN", `x-content-type-options` = "nosniff",
      etag = "W/\"0677758b252ee5d23494cd16083e51bdf\"",
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
  ), class = "data.frame"), content = charToRaw("{\"organisationUnitGroups\":[{\"code\":\"CHP\",\"id\":\"uYxK4wmcPqA\"},{\"id\":\"GGghZsfu7qV\"}]}"),
  date = structure(1589973388, class = c("POSIXct", "POSIXt"), tzone = "GMT"), times = c(
    redirect = 0.040263, namelookup = 3.2e-05,
    connect = 3.4e-05, pretransfer = 6.2e-05, starttransfer = 0.047366,
    total = 0.087673
  )
), class = "response")
