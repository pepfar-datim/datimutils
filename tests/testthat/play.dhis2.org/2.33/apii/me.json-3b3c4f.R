structure(list(url = "https://play.dhis2.org/2.33.6/apii/me.json?paging=false", 
    status_code = 401L, headers = structure(list(server = "nginx/1.17.10", 
        date = "Tue, 17 Nov 2020 11:13:12 GMT", `content-type` = "application/json;charset=UTF-8", 
        `content-length` = "92", `x-xss-protection` = "1; mode=block", 
        `x-frame-options` = "SAMEORIGIN", `x-content-type-options` = "nosniff"), class = c("insensitive", 
    "list")), all_headers = list(list(status = 302L, version = "HTTP/2", 
        headers = structure(list(server = "nginx/1.17.10", date = "Tue, 17 Nov 2020 11:13:11 GMT", 
            `content-type` = "text/html", `content-length` = "146", 
            location = "https://play.dhis2.org/2.33.6/apii/me.json?paging=false", 
            `strict-transport-security` = "max-age=15768000"), class = c("insensitive", 
        "list"))), list(status = 401L, version = "HTTP/2", headers = structure(list(
        server = "nginx/1.17.10", date = "Tue, 17 Nov 2020 11:13:12 GMT", 
        `content-type` = "application/json;charset=UTF-8", `content-length` = "92", 
        `x-xss-protection` = "1; mode=block", `x-frame-options` = "SAMEORIGIN", 
        `x-content-type-options` = "nosniff"), class = c("insensitive", 
    "list")))), cookies = structure(list(domain = "#HttpOnly_play.dhis2.org", 
        flag = FALSE, path = "/2.33.6", secure = TRUE, expiration = structure(Inf, class = c("POSIXct", 
        "POSIXt")), name = "JSESSIONID", value = "REDACTED"), row.names = c(NA, 
    -1L), class = "data.frame"), content = charToRaw("{\"httpStatus\":\"Unauthorized\",\"httpStatusCode\":401,\"status\":\"ERROR\",\"message\":\"Unauthorized\"}"), 
    date = structure(1605611592, class = c("POSIXct", "POSIXt"
    ), tzone = "GMT"), times = c(redirect = 0.040857, namelookup = 0.00014, 
    connect = 0.000145, pretransfer = 0.000342, starttransfer = 0.159096, 
    total = 0.159185)), class = "response")
