# removes any nested fields (and spaces) from a character vector of field names
# e.g. "indicators[name]" becomes "indicators", "id,indicators[name,id]"
# becomes "id,indicators" and c("indicators[id]","organisationUnits[name]")
# becomes c("indicators", "organisationUnits")
.topLevelFields <-  function(strings){

  if(!is.character(strings)){
    stop(paste("The internal function .removeBracketedText expected",
               "a character vector but recieved an oject of type:",
               typeof(strings)))
  }

# fields can only contain letters, square brackets and commas
# we remove white space here as well
  strings <- stringr::str_remove_all(strings, " ")
  if(any(stringr::str_detect(strings, "[^a-zA-Z,\\[\\]]"))){
    stop(paste("The internal function .removeBracketedText received a string",
               "with a character other than a-zA-Z, [] or a literal comma(,)"))
  }

  while (any(stringr::str_detect(strings, "\\[") &
             stringr::str_detect(strings, "]"))) {
    strings <- stringr::str_remove_all(strings,
                                       "\\[[a-zA-Z,]*\\]")
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
  function_name <- "getOrgUnitGroups"
  end_point <-  "organisationUnitGroups"
  identifiable_properties <- c("name", "id", "code", "shortName")

# by can come in as string or NSE, convert to string
  by <- rlang::as_string(rlang::ensym(by))

# by parameter restricted to being an identifiable property
# as defined in DHIS2 docs
  if (!(by %in% identifiable_properties)){
    stop(paste0(function_name,
                " expects a by parameter of id (the default), name, code, or ",
                "shortName. Use the ",
                "more general getMetadata function for other scenarios."
    ))
  }

  name_reduce <- NULL
  default_fields <- if(is.null(fields)){
    c("name", "id")} else {fields}

  by_ns <- try(rlang::ensym(by), silent = TRUE)

  if (is.null(fields) && class(by_ns) == "try-error") {
    name_reduce <- "name"
  } else if (by_ns == "name" & is.null(fields)) {
    name_reduce <- "id"
  }

  # process first filter item (id, name, etc.)
  default_filter_item <- ifelse(class(by_ns) == "try-error", "id", by_ns)
  # process field options
  default_fields <- if (by == "name" & is.null(fields)) {
    "id"
  } else if (is.null(fields)) {
    "name"
  } else {
# no property names have spaces so remove any whitespace in fields
    stringr::str_remove_all(fields, " ")
  }

# check if fields contains by as a distinct element
# \\b represents a non word character so by must be a distinct element
# TODO fix logic here which would say id is in fields if fields was name,organisationUnits[name,id]
  by_in_fields <- any(grepl(paste0("\\b", by, "\\b"),
                            .topLevelFields(default_fields)))

# in order to ensure matching with input vector we must have the by
# column in our results, so add to fields if not requested
  if (!by_in_fields){
    default_fields <- c(by, default_fields)
  }

  # make filters
  unique_values <- unique(values)

  filters <- datimutils::metadataFilter(values = unique_values,
                                        property = by,
                                        operator = "in")

  # make dataframe to know how to expand result in case of duplicate filters
  n_occur <- data.frame(table(values), stringsAsFactors = F)
  n_occur <- n_occur[match(unique_values, n_occur$values), ]
  filters <- paste0(default_filter_item, ":", default_filter_option, ":", paste0(unique(values), collapse = ","))

  # call getMetadata with info above
  data <- getMetadata(end_point = !!end_point,
              base_url = base_url,
              filters,
              fields = default_fields, pluck = F, retry = 1,
              expand = n_occur, name_reduce = name_reduce
  )
}

#' #' @export
#' #' @title getOrgUnitGroups2(filters1 = NULL, filters2 = NULL, fields = NULL,
#' #' base_url = NULL, by1 = NULL, by2 = NULL)
#' #' @description wrapper to getMetadata that retrieves org units
#' #' @param base_url string - base address of instance (text before api/ in URL)
#' #' @param filters1 - the filters, which can come in any format as long as all
#' #' components are present
#' #' @param filters2 - the filters, which can come in any format as long as all
#' #' components are present
#' #' @param fields - the fields, which can come in any formt as long as all
#' #' components are present
#' #' @param by1 - what to filter by, i.e. id or name, default is id,
#' #' applies to filter1
#' #' @param by2 - what to filter by, i.e. id or name, default is id,
#' #' applies to filter2
#' #' @return the metadata response in json format and flattened
#'
#' getOrgUnitGroups2 <- function(filters1 = NULL, filters2 = NULL,
#'                               by1 = NULL, by2 = NULL,
#'                               fields = NULL, base_url = getOption("baseurl")) {
#'   # process field options
#'   default_fields <- if (is.null(fields)) {
#'     c("name", "id")
#'   } else {
#'     fields
#'   }
#'   # process first filter item (id, name, etc.)
#'   default_filter_item1 <- ifelse(is.null(by1), "id", by1)
#'   # process first filter option (in, eq, like, etc.)
#'   default_filter_option1 <- "in"
#'   # process second filter item (id, name, etc.)
#'   default_filter_item2 <- ifelse(is.null(by2), "id", by2)
#'   # process second filter option (in, eq, like, etc.)
#'   default_filter_option2 <- "in"
#'   # call getMetadata with info above
#'   getMetadata(
#'     base_url = base_url, end_point = "organisationUnitGroups",
#'     filters = list(
#'       paste0(default_filter_item1, default_filter_option1, paste0(filters1, collapse = ",")),
#'       paste0(default_filter_item2, default_filter_option2, paste0(filters2, collapse = ","))
#'     ),
#'     fields = default_fields, pluck = F, retry = 1, expand = NULL
#'   )
#' }
