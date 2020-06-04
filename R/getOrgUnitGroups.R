#' @export
#' @title getOrgUnitGroups(filters = NULL, fields = NULL,
#' base_url = NULL, by = NULL)
#'
#' @description wrapper to getMetadata that retrieves org units
#' @param x - string vector of identifiers that will be used in the filter
#' e.g. vector of uids, names, or codes.
#' @param by - what to filter by, i.e. id or name, default is id
#' @param fields - the fields, which can come in any formt as long as all
#' components are present
#' @param base_url string - base address of instance (text before api/ in URL)
#' @return the metadata response in json format and flattened
#'
getOrgUnitGroups <- function(x = NULL, by = NULL, fields = NULL,
                             base_url = getOption("baseurl")) {
  # process field options
  default_feilds <- if (is.null(fields)) {
    c("name", "id")
  } else {
    fields
  }
  # process first filter item (id, name, etc.)
  default_filter_item <- ifelse(is.null(by), "id", by)
  # process first filter option (in, eq, like, etc.)
  default_filter_option <- "in"

  # make filters
  uniquex <- unique(x)

  # this option is more robust but would need to change mocks
  # filters = paste0(default_filter_item, default_filter_option, uniquex)

  filters <- paste0(default_filter_item, default_filter_option, paste0(uniquex, collapse = ","))

  # make dataframe to know how to expand result in case of duplicate filters
  n_occur <- data.frame(table(x), stringsAsFactors = F)
  n_occur <- n_occur[match(uniquex, n_occur$x), ]

  if (all(n_occur$Freq == 1)) {
    n_occur <- NULL
  }

  # call getMetadata with info above
  getMetadata(
    end_point = "organisationUnitGroups", base_url = base_url,
    filters = filters,
    fields = default_feilds, pluck = F, retry = 1,
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
#'   default_feilds <- if (is.null(fields)) {
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
#'     fields = default_feilds, pluck = F, retry = 1, expand = NULL
#'   )
#' }
