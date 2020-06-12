#' @export
#' @title getOrgUnitGroups
#'
#' @description wrapper to getMetadata that retrieves org units
#' @param values - string vector of identifiers that will be used in the filter
#' e.g. vector of uids, names, or codes.
#' @param by - what to filter by, i.e. id or name, default is id
#' @param fields - the fields, which can come in any formt as long as all
#' components are present
#' @param strict boolean - when strict is TRUE, the default, this function
#' requires a 1:1 mapping between the input values and the api results. Any values
#' missing from the API response are inserted as NA. When true the by parameter 
#' must be one of the identifiable properties {name, code, id, shortName}, 
#' note id is the default by parameter
#' @param base_url string - base address of instance (text before api/ in URL)
#' @return the metadata response in json format and flattened
#'
getOrgUnitGroups <- function(values = NULL, 
                             by = "id", 
                             fields = NULL,
                             strict = TRUE,
                             base_url = getOption("baseurl")) {
  function_name <- "getOrgUnitGroups"
  end_point <-  "organisationUnitGroups"

  default_filter_item <- rlang::as_string(rlang::ensym(by))
  identifiable_properties <- c("name", "id", "code", "shortName")
  
  if (!(default_filter_item %in% identifiable_properties) &&  
        strict == TRUE){
    stop(paste0("When strict = TRUE ", getOrgUnitGroups,
                " expects a by parameter of id (the default), name, code, or ",
                "shortName. Consider setting strict = FALSE, or utilizing the ",
                "more general getMetadata function."
    ))
  }
  
  # process field options
  default_fields <- if (default_filter_item == "name" & is.null(fields)) {
    "id"
  } else if (is.null(fields)) {
    "name"
  } else {fields}

  # make filters
  unique_values <- unique(values)

  # this option is more robust but would need to change mocks
  # filters = paste0(default_filter_item, default_filter_option, unique_values)

  filters <- datimutils::metadataFilter(unique_values,
                                        default_filter_item,
                                        "in")
     print(filters)
  # make dataframe to know how to expand result in case of duplicate filters
  n_occur <- data.frame(table(values), stringsAsFactors = F)
  n_occur <- n_occur[match(unique_values, n_occur$values), ]

  # call getMetadata with info above
  getMetadata(
    end_point = !!end_point, base_url = base_url,
    filters,
    fields = default_fields, pluck = F, retry = 1,
    expand = n_occur
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
