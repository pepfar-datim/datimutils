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
getOrgUnitGroups <- function(values, 
                             by = "id", 
                             fields = NULL,
                             base_url = getOption("baseurl")) {
  function_name <- "getOrgUnitGroups"
  end_point <-  "organisationUnitGroups"

  default_filter_item <- rlang::as_string(rlang::ensym(by))
  
  identifiable_properties <- c("name", "id", "code", "shortName")
  
  if (!(default_filter_item %in% identifiable_properties)){
    stop(paste0(function_name,
                " expects a by parameter of id (the default), name, code, or ",
                "shortName. Use the ",
                "more general getMetadata function for other scenarios."
    ))
  }
  
  # process field options
  default_fields <- if (default_filter_item == "name" & is.null(fields)) {
    "id"
  } else if (is.null(fields)) {
    "name"
  } else {
# no property names have spaces so remove any whitespace in fields
    stringr::str_remove(fields, " ")
  } 
 
# check if fields contains by as a distinct element
# \\b represents a non word character so by must be a distinct element
  by_in_fields <- any(grepl(paste0("\\b", default_filter_item, "\\b"), 
                            default_fields))

  print(by_in_fields)
  # in order to ensure matching with input vector we must have the by 
# column in our results  
  if (!by_in_fields){
    default_fields <- c(default_filter_item, default_fields)
  }
  
  # make filters
  unique_values <- unique(values)

  filters <- datimutils::metadataFilter(unique_values,
                                        default_filter_item,
                                        "in")
  
  # make dataframe to know how to expand result in case of duplicate filters
  n_occur <- data.frame(table(values), stringsAsFactors = F)
  n_occur <- n_occur[match(unique_values, n_occur$values), ]

  # call getMetadata with info above
  data <- getMetadata(end_point = !!end_point, 
              base_url = base_url,
              filters,
              fields = default_fields, pluck = F, retry = 1,
              expand = n_occur
  )
  
#  put values in a data frame
  values <- stats::setNames(data.frame(values), default_filter_item)

# join results to input vector columns will have the same name as the
# are a common property
print(names(data))
print(names(values))
  data <- dplyr::left_join(values, data, copy = TRUE)
  
  if (!by_in_fields){
    return(dplyr::select(data, -default_filter_item))
  }
  return(data)
  
  # if (NROW(data) != length(values)){
  #   stop(paste0(function_name, " resulting in a different number",
  #               "of input and output items"))
  # }
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
