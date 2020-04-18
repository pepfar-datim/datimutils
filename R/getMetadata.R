#' @export
#' @title getMetadata(base_url, end_point, filters, fields, pluck)
#'
#' @description General utility to get metadata details from DATIM
#' @param end_point string - api endpoint for the metadata of interest
#' e.g. dataElements, organisationUnits
#' @param base_url string - base address of instance (text before api/ in URL)
#' @param filters - the filters, which can come in any format as long as all
#' components are present
#' @param fields - the fields, which can come in any formt as long as all
#' components are present
#' @param pluck - whether to add pluck option as documented by dhis2 api
#' developer guide
#' @param retry number of times to retry
#' @return the metadata response in json format and flattened
#'

getMetadata <- function(end_point, base_url = getOption("baseurl"),
                        filters = NULL, fields = NULL,
                        pluck = F, retry = 1) {
  #if no filters or fields are specified, just use endpoint as path
  if (!(is.null(filters)) | !(is.null(fields))) {
    end_point <- gsub("/", "", end_point)
  }
  #filter block
  if (!(is.null(filters))) {
    #takes filter argument and turns it into a single character string
    ex <- stringr::str_flatten(unlist(sapply(filters, as.character)))
    #removes extraneous info
    look <- sub("\\?filter=", "", ex)
    look <- sub("\\&filter=", "", ex)
    #extracts end_point and what is not end_point
    if (!(grepl("^id|name", look))) {
      end_point <- stringr::str_extract(look, ".+?(?=id|name)")
      look <- sub("(.*)(\\id|name)", "\\2", look)
      end_point <- gsub("/", "", end_point)
    }
    #extracts the original filter 
    filter_option_orig <-
      stringr::str_extract(substr(look, 1, 10),
                           "!ilike|!like|ilike|!in|!eq|in|eq|like")
    #extracts either id or name from filter
    filter_item <- stringr::str_extract(substr(look, 1, 10), "id|name")
    #this block replaces the filter with one more adequate (eq=in, like=ilike, etc.)
    if (grepl("eq", filter_option_orig)) {
      filter_option <- sub("eq", "in", filter_option_orig)
    } else if ("like" == filter_option_orig) {
      filter_option <- sub("like", "ilike", filter_option_orig)
    } else {
      filter_option <- filter_option_orig
    }
    #creates a basic filter path
    ex <- paste0(gsub(filter_option_orig, filter_option, substr(look, 1, 5)),
                 substr(look, 6, nchar(look)))
    #removes :
    ex <- gsub(":", "", ex)
    #takes first part of filter, i.e. idin
    one <- sub(paste0("(", filter_item, filter_option, ")", "(.*)"),
               "\\1", ex)
    one.one <- sub(paste0("(", filter_item, filter_option, ")", "(.*)"),
                   "\\2", ex)
    #takes second part of filter, i.e. ["abc"] and adds commas if filter = in
    two <- ifelse(grepl(",", one.one), one.one,
                  gsub("(.{11})", "\\1,",
                       one.one, perl = TRUE))
    ex <- paste0(end_point, one, two)
    if (substr(ex, nchar(ex), nchar(ex)) == ",") {
      ex <- substr(ex, 1, nchar(ex) - 1)
    }
    #adds &filter= where needed, and : where needed
    ex <- sub(paste0("(.*?)(\\&filter=|", filter_item, "|?filter=", ")"),
              paste0("\\1&filter=", filter_item), ex)
    ex <- sub(paste0("(.*?)", "(", filter_item, ")"),
              paste0("\\1", filter_item, ":"), ex)
    ex <- sub(paste0("(.*?)", "(", filter_option, ")"),
              paste0("\\1", filter_option, ":"), ex)
    #special processing for "in" filter
    if (grepl("in", filter_option) & !(grepl("\\[", ex))) {
      ex <- paste0(sub("(.*?)(in:)", "\\1in:[", ex), "]")
    }
    #removes whitespace
    ex <- gsub(" ", "", ex)
  }
  if (!(is.null(fields))) {
    #flattens fields and adds ?fields= if needed
    ef <- stringr::str_flatten(unlist(sapply(fields, as.character)), ",")
    if (!(grepl("fields", ef))) {
      ef <- paste0("&fields=", ef)
    }
  }
  #create final path
  path <- paste0(ex, ef, ifelse(pluck, "~pluck", ""))
  if (is.null(fields) & is.null(filters)) {
    path <- end_point
  }
  #pass path in api_get
  api_get(
    path = path, base_url = base_url, retry = retry, timeout = 60,
    api_version = NULL
  )
}
