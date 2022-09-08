#' @export
#' @title getDataValueSets
#'
#' @description Used to read DHIS 2 data using the data value set endpoint
#' @param keys character vector - data value set parameter keys
#'  (e.g. "dataSet", "period")
#' @param values character vector - values marching the key from keys
#'  (e.g. "Abcde123456", "2019Q1"
#' @param d2_session R6 datimutils object which handles authentication
#'  with DATIM
#' @param retry number of times to retry
#' @param timeout number of seconds to wait during call
#' @param verbose return raw content with data
#' @param quiet Echo the URL which is called to the console if TRUE.
#' @return  Data frame with the data requested
#'
getDataValueSets <- function(variable_keys = NULL, #keys,
                             variable_values = NULL, #values,
                             d2_session = dynGet("d2_default_session",
                                                 inherits = TRUE),
                             retry = 1, timeout = 180,
                             verbose = FALSE,
                             quiet = TRUE) {

  #Test that the provided variables have their associated values used for
  # munging
  assertthat::assert_that(length(variable_keys) == length(variable_values),
    msg = "The number of keys provided is not equal to the number of values provided.
    Please ensure these match and try again.")

  #Example api call
  #/api/dataValueSets.json?dataSet=pBOMPrpg1QX&period=201401&orgUnit=DiszpKrYNg8

  #TODO: Consider implementing a check of all paramaters
  #https://docs.dhis2.org/en/develop/using-the-api/dhis-core-version-master/data.html

  #Requirements
  # The following constraints apply to the data value sets resource:

  #1 At least one data set must be specified OR a dataElementGroup.
  if (!(is.element("dataSet", variable_keys)) == TRUE &&
      !(is.element("dataElementGroup", variable_keys)) == TRUE) {
    stop("At least one data set or data element group must be specified.")
  }

  #2 Either at least one period or a start date and end date must be specified.
  if ((
    (!(is.element("startDate", variable_keys)) ||
     !(is.element("endDate", variable_keys)))
    &&
    !(is.element("period", variable_keys))
    )) {
    stop("Either at least one period or a start date and end date must be
         specified.")
  }

  #3 At least one organisation unit must be specified.
  if (!(is.element("orgUnit", variable_keys)) == TRUE &&
      !(is.element("orgUnitGroup", variable_keys)) == TRUE) {
    stop("At least one organisation unit or organisation unit group must be specified.")
  }

  #4 Organisation units must be within the hierarchy of the organisation units
  #    of the authenticated user.
    # This should be taken care of by loginToDatim, so wont add at this time.

  #5 Limit cannot be less than zero.
    # I don't think we need to bother coding this

  # Removed ifelse below, as it should theoretically be impossible to not
  #    enter variables with the above stop's
  # concatenate and format the keys and values provided for the api call
    variable_k_v_pairs <- mapply(function(x, y) paste0(x, "=", y),
                                 variable_keys, variable_values)
    variable_k_v_pairs <- paste0(variable_k_v_pairs, collapse = "&")

    #Create URL Path
    path <- paste0("api/dataValueSets.json?",
         variable_k_v_pairs,
         "&paging=false")

    resp <- api_get(
      path = path,
      d2_session = d2_session,
      retry = retry,
      timeout = timeout,
      verbose = verbose,
      quiet = quiet
    )

    if (verbose) {
      meta_data <- resp$api_responses
      resp <- resp$data
    }

    #Create Dataframe from api response
    resp <- as.data.frame(resp$dataValues, stringsAsFactors = FALSE)

    if (verbose) {
      return(list("data" = resp, "api_responses" = meta_data))
    } else {
      return(resp)
    }
}
