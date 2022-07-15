#' @title simplifyStructure(resp)
#' @description takes a api response and simplifies it down to the most basic data structure
#' @param resp raw text response recieved from datim api
#' @return api response reduced to most simple data structure
#'
simplifyStructure <- function(resp) {

  # only enter if class is list and length one, otherwise it is already simplified
  if (class(resp) == "list" &
    length(resp) == 1 &
    length(resp[[1]]) != 0) {
    possible_resp <- resp
    continue <- T

    # the while block reduces the structure till it can't
    while (continue) {
      if (class(possible_resp) == "list") {
        possible_resp <- possible_resp[[1]]
        dim1 <- dim(possible_resp)[1]
        dim2 <- dim(possible_resp)[2]
        if (!(is.null(dim1)) && !(is.null(dim2))) {
       if (dim1 == 1 & dim2 == 1) {
        possible_resp <- possible_resp[[1]]
      }
      }}else {
        continue <- F
      }}


    # if it is a data frame check if it is nested or standard
    if (class(possible_resp) == "data.frame") {
      if (!(("list" %in% apply(possible_resp, 2, typeof)))) {
        resp <- possible_resp
      } else {
        # unnest dataframe if has list type in columns
        if (!(length(possible_resp[, sapply(possible_resp, class) == "list"][[1]]) == 0)  & ncol(possible_resp) == 1) {
          resp <- try(tidyr::unnest(possible_resp, cols = colnames(possible_resp)), silent = T)
        } else {
          resp <- possible_resp
        }
      }
    } else if (is.atomic(possible_resp)) {
      resp <- possible_resp
    }
  }
  return(resp)
}


#' @export
#' @title getMetadata
#' @description General utility to get metadata details from DATIM
#' @param end_point string - api endpoint for the metadata of interest
#' e.g. dataElements, organisationUnits. Non-standard evaluation supported.
#' @param ... - one or more metadata filters specified as a combination of
#' strings and/or character vectors, eg:
#' \preformatted{
#' "name:!eq:ANC", "indicators.name:like:ANC"}
#' or
#' \preformatted{c("name:!eq:ANC", "indicators.name:like:ANC"),
#' "id:!in:[a11111111111,b22222222222]"
#' }
#' see datimutils::metadataFilter and related helpers
#' @param fields - the metadata fields requested as a comma
#' seperated string or character vector, eg:
#' \preformatted{
#' "name, id"}
#' or
#' \preformatted{
#' c("name", "id")}
#' #' or
#' \preformatted{
#' c("name,id", "code")}
#' @param as_vector attempt to return an atomic vector when only a single field
#' is requested and returned. Defaults to TRUE.
#' @param d2_session the d2Session object, default is "d2_default_session",
#' it will be made upon logining in to datim with loginToDATIM
#' @param retry number of times to retry
#' @param timeout integer - seconds to wait for a response, default = 180
#' @return the metadata response in json format and flattened
#'

getMetadata <- function(end_point,
                        ...,
                        fields = "name,id",
                        as_vector = T,
                        d2_session = dynGet("d2_default_session",
                                            inherits = TRUE),
                        retry = 1,
                        timeout = 180) {
  if (!is.character(fields)) {
    stop("The fields argument of getMetadata should be of type character")
  }

  # non-standard evaluation for end_point convert to string
  end_point <- as.character(rlang::ensym(end_point))
  if (end_point == "") {
    stop("end_point must be specified for getMetadata to run.")
  }


  # set up storage for multiple filter arguments
  filter_storage <- list()

  # process filter arguments

  if (missing(...)) {
    ex <- NULL
  } else {
    # turn filters recieved as ... to a character vector of individual filters
    filters_chr <- unlist(list(...))
    ex <- stringi::stri_flatten(filters_chr, "&filter=")
    ex <- paste0("&filter=", ex)
  }

  # flattens fields and adds ?fields= if needed
  ef <- stringi::stri_flatten(fields, ",")
  ef <- paste0("&fields=", ef)

  # create final path
  path <- paste0(end_point, ex, ef)
  # pass path in api_get
  resp <- api_get(
    path = path, d2_session = d2_session, retry = retry,
    timeout = timeout,
    api_version = NULL
  )

   # simplify data structure
  resp <- simplifyStructure(resp)

  # do we have single value to return?
  if (is.atomic(resp) && length(resp) == 1) {
    return(resp)
  }

  # If we only request one singular field and that is what we got back
  # return atomic vector unless as_vector = FALSE
  # when reaching in to collection handle the fact that the returned name
  # is in []
  if (as_vector == TRUE &&
    NCOL(resp) == 1 &&
    length(fields) == 1 &&
    !grepl(",", fields) &&
    (
      names(resp) == fields ||
        grepl(paste0("[", names(resp), "]"),
          fields,
          fixed = TRUE
        )
    )) {
    return(resp[[1]])
  }

  return(resp)
}

#' @export
#' @title metadataFilter(values, property, operator)
#' @description used to format filter strings for metadata calls
#' @param values the values in property:operator:value
#' @param property the property in property:operator:value
#' @param operator the operator in property:operator:value
#' @return property:operator:value
#' @usage
#'
#' metadataFilter(values, property, operator)
#'
#' property %deq% value
#'
#' property %d!eq% value
#'
#' property %dlike% value
#'
#' property %d!like% value
#'
#' property %din% values
#'
#' property %d!in% values

metadataFilter <- function(values, property, operator) {

  # check values is a vector only for in and !in operators
  if (length(values) > 1 &&
    !(operator %in% c("in", "!in"))) {
    stop("A vector of values is only supported for in and !in operators")
  }

  if (is.null(values) &&
    !(operator %in% c("null", "!null", "empty"))) {
    stop("NULL values are only supported for null, !null and empty operators")
  }

  if (!is.null(values) &&
    operator %in% c("null", "!null", "empty")) {
    stop("NULL values required for null, !null and empty operators")
  }

  if (operator %in% c("in", "!in")) {
    return(paste0(
      property, ":", operator, ":[",
      paste0(values, collapse = ","),
      "]"
    ))
  } else {
    return(paste0(property, ":", operator, ":", values))
  }
}

#' @export
#' @rdname metadataFilter
"%.in%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "in")
}

#' @export
#' @rdname metadataFilter
"%.~in%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "!in")
}

#' @export
#' @rdname metadataFilter
"%.token%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "token")
}

#' @export
#' @rdname metadataFilter
"%.~token%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "!token")
}

#' @export
#' @rdname metadataFilter
"%.le%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "le")
}

#' @export
#' @rdname metadataFilter
"%.lt%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "lt")
}

#' @export
#' @rdname metadataFilter
"%.ge%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "ge")
}

#' @export
#' @rdname metadataFilter
"%.gt%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "gt")
}


#' @export
#' @rdname metadataFilter
"%.Like%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "like")
}

#' @export
#' @rdname metadataFilter
"%.~Like%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "!like")
}

#' @export
#' @rdname metadataFilter
"%.^Like%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "$like")
}

#' @export
#' @rdname metadataFilter
"%.~^Like%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "!$like")
}

#' @export
#' @rdname metadataFilter
"%.Like$%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "like$")
}

#' @export
#' @rdname metadataFilter
"%.~Like$%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "!like$")
}

#' @export
#' @rdname metadataFilter
"%.like%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "ilike")
}

#' @export
#' @rdname metadataFilter
"%.^like%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "!ilike")
}

#' @export
#' @rdname metadataFilter
"%.~^like%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "!$ilike")
}

#' @export
#' @rdname metadataFilter
"%.~like%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "!ilike")
}

#' @export
#' @rdname metadataFilter
"%.like$%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "ilike$")
}

#' @export
#' @rdname metadataFilter
"%.~like$%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "!ilike$")
}

#' @export
#' @rdname metadataFilter
"%.eq%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "eq")
}

#' @export
#' @rdname metadataFilter
"%.~eq%" <- function(property, values) {
  property <- rlang::ensym(property)
  metadataFilter(values, property, "!eq")
}
