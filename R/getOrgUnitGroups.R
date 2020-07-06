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

  # by can come in as string or NSE, convert to string
  by <- try(as.character(rlang::ensym(by)), silent = TRUE)

  default_fields <- if (is.null(fields)) {
    c(by, "name", "id")
  } else if (!("name" %in% fields) && !(any(grepl("name", fields)))) {
    c(by, fields, "name")
  } else{
    fields
  }
  default_fields <- stringr::str_remove_all(default_fields, " ")
  default_fields <- unique(default_fields)
  # by parameter restricted to being an identifiable property
  # as defined in DHIS2 docs
  if (!(by %in% c("name", "id", "code", "shortName"))) {
    stop(
      "getOrgUnits expects a by parameter of id (the default), name, code, or ",
      "shortName. Use the ",
      "more general getMetadata function for other scenarios."
    )
  }

  if ((by == "name" & is.null(fields))) {
    name_reduce <- "id"
  } else if (is.null(fields)) {
    name_reduce <- "name"
  } else if (!(is.null(fields))){
    name_reduce <-  gsub("\\[[^()]*\\]", "",fields)
    if(length(name_reduce == 1)){
      name_reduce <- gsub(" ", "", unlist(strsplit(name_reduce, ",")))
    }
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
