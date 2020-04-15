#' @export
#' @title getMetadata(base_url, end_point, filters, fields, pluck)
#' 
#' @description General utility to get metadata details from DATIM
#' @param base_url string - base address of instance (text before api/ in URL)
#' @param end_point string - api endpoint for the metadata of interest e.g. dataElements, 
#' organisationUnits
#' @param filters - the filters, which can come in any format as long as all components are present
#' @param fields - the fields, which can come in any formt as long as all components are present
#' @param pluck - whether to add pluck option as documented by dhis2 api developer guide
#' @param retry number of times to retry
#' @return the metadata response in json format and flattened
#'

getMetadata <- function(base_url = getOption("baseurl"), 
                        end_point, filters = NULL, fields = NULL,
                        pluck = F, retry = 1) {
  if(!(is.null(filters)) | !(is.null(fields))){end_point <- gsub("/", "", end_point)}
  if (!(is.null(filters))){
    ex <- stringr::str_flatten(unlist(sapply(filters, as.character)))
    look <- sub("\\?filter=", "", ex)
    if(!(grepl("^id|name", look))){
      end_point <- stringr::str_extract(look, ".+?(?=id|name)")
      look <- sub("(.*)(\\id|name)", "\\2", look)
      end_point <- gsub("/", "", end_point)
    }
    filter_option_orig <- stringr::str_extract(substr(look,1,10),"!ilike|!like|ilike|!in|!eq|in|eq|like")
    filter_item <- stringr::str_extract(substr(look,1,10),"id|name")
  if (grepl("eq",filter_option_orig)){filter_option <- sub("eq","in",filter_option_orig)
  }else if("like" == filter_option_orig){filter_option <- sub("like","ilike",filter_option_orig)
  }else{filter_option <- filter_option_orig}
  ex <- paste0(gsub(filter_option_orig, filter_option, substr(look,1,5)),substr(look,6,nchar(look)) )
  ex <- gsub(":", "", ex)
  one <- sub(paste0("(",filter_item, filter_option,")","(.*)"), "\\1", ex)
  one.one <- sub(paste0("(",filter_item, filter_option,")","(.*)"), "\\2", ex)
  two <- ifelse(grepl(",",one.one), one.one, gsub('(.{11})', "\\1,", one.one , perl = TRUE))
  ex <- paste0(end_point, one,two)
  if(substr(ex,nchar(ex),nchar(ex)) == ","){ex <- substr(ex,1,nchar(ex)-1)}
  ex <- sub(paste0("(.*?)(\\?filter=|",filter_item, ")"), paste0("\\1?filter=", filter_item), ex)
  ex <- sub(paste0("(.*?)","(",filter_item,")"), paste0("\\1",filter_item, ":"), ex)
  ex <- sub(paste0("(.*?)","(",filter_option,")"), paste0("\\1",filter_option, ":"), ex)
  if(grepl("in",filter_option)&!(grepl("\\[", ex))){ex <- paste0(sub("(.*?)(in:)", "\\1in:[", ex),"]")}
  ex <- gsub(" ", "", ex)
  }
  if (!(is.null(fields))){
  ef <- stringr::str_flatten(unlist(sapply(fields, as.character)),",")
  if(!(grepl('fields', ef))){ef <- paste0("?fields=", ef)}
  }

  path <- paste0(ex,ef, ifelse(pluck, "~pluck", ""))
  if(is.null(fields) & is.null(filters)){path <- end_point}
  api_get(path = path, base_url = base_url, retry = retry, timeout = 60,
                      api_version = NULL )
  }
