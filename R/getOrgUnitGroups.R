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
getOrgUnitGroups <- function(values = NULL, by = NULL, fields = NULL,
                             base_url = getOption("baseurl")) {
  
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

  # process first filter option (in, eq, like, etc.)
  default_filter_option <- "in"

  # make filters
  filters <- paste0(default_filter_item, ":", default_filter_option, ":", paste0(unique(values), collapse = ","))

  # call getMetadata with info above
  getMetadata(
    end_point = "organisationUnitGroups", base_url = base_url,
    filters,
    fields = default_fields, pluck = F, retry = 1,
    expand = values, name_reduce = name_reduce
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
