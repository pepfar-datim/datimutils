# removes any nested fields (and spaces) from a character vector of field names
# e.g. "indicators[name]" becomes "indicators", "id,indicators[name,id]"
# becomes "id,indicators" and c("indicators[id]","organisationUnits[name]")
# becomes c("indicators", "organisationUnits")
.topLevelFields <- function(strings) {
  if (!is.character(strings)) {
    stop(paste(
      "The internal function .removeBracketedText expected",
      "a character vector but recieved an oject of type:",
      typeof(strings)
    ))
  }

  # fields can only contain letters, square brackets and commas
  # we remove white space here as well
  strings <- stringr::str_remove_all(strings, " ")
  if (any(stringr::str_detect(strings, "[^a-zA-Z,\\[\\]]"))) {
    stop(paste(
      "The internal function .removeBracketedText received a string",
      "with a character other than a-zA-Z, [] or a literal comma(,)"
    ))
  }

  while (any(stringr::str_detect(strings, "\\[") &
    stringr::str_detect(strings, "]"))) {
    strings <- stringr::str_remove_all(
      strings,
      "\\[[a-zA-Z,]*\\]"
    )
  }
  return(strings)
}

#' @export
#' @title getOrgUnitGroups
#'
#' @description wrapper to getMetadata that retrieves org units
#' @param values - string vector of identifiers that will be used in the filter
#' e.g. vector of uids, names, or codes.
#' @param by - what to filter by, i.e. id or name, default is id
#' @param fields - the fields, which can come in any formt as long as all
#' components are present
#' @param base_url string - base address of instance (text before api/ in URL)
#' @return the metadata response in json format and flattened
#'
getOrgUnitGroups <- function(values,
                             by = "id",
                             fields = NULL,
                             base_url = getOption("baseurl")) {
  name_reduce <- NULL
  default_fields <- if (is.null(fields)) {
    c("name", "id")
  } else {
    stringr::str_remove_all(fields, " ")
  }

  # by can come in as string or NSE, convert to string
  by <- try(as.character(rlang::ensym(by)), silent = TRUE)

  # by parameter restricted to being an identifiable property
  # as defined in DHIS2 docs
  if (!(by %in% c("name", "id", "code", "shortName"))) {
    stop(
      "getOrgUnits expects a by parameter of id (the default), name, code, or ",
      "shortName. Use the ",
      "more general getMetadata function for other scenarios."
    )
  }

  if (by == "id" & is.null(fields)) {
    name_reduce <- "name"
  } else if (by == "name" & is.null(fields)) {
    name_reduce <- "id"
  }

  # check if fields contains by as a distinct element
  # \\b represents a non word character so by must be a distinct element
  # TODO fix logic here which would say id is in fields if fields was name,organisationUnits[name,id]
  by_in_fields <- any(grepl(
    paste0("\\b", by, "\\b"),
    .topLevelFields(default_fields)
  ))

  # in order to ensure matching with input vector we must have the by
  # column in our results, so add to fields if not requested
  if (!by_in_fields) {
    default_fields <- c(by, default_fields)
  }

  filters <- datimutils::metadataFilter(
    values = unique(values),
    property = by,
    operator = "in"
  )

  # call getMetadata with info above
  data <- getMetadata(
    end_point = "organisationUnitGroups",
    base_url = base_url,
    filters,
    fields = default_fields, pluck = F, retry = 1,
    expand = values, name_reduce = name_reduce
  )
}
