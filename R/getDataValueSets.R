#' @export
#' @title getDataValueSets
#'
#' @description Used to read DHIS 2 data using the data value set endpoint
#' @param keys character vector - data value set parameter keys (e.g. "dataSet", "period")
#' @param values character vector - values marching the key from keys (e.g. "Abcde123456", "2019Q1"
#' @param d2_session R6 datimutils object which handles authentication with DATIM
#' @param retry number of times to retry
#' @param timeout number of seconds to wait during call
#' @return  tibble with the data requested
#'
getDataValueSets <- function(variable_keys = NULL, #keys,
                             variable_values = NULL, #values,
                             d2_session = dynGet("d2_default_session",
                                                 inherits = TRUE),
                             retry=1, timeout = 180) {
  
  #Test that the provided variables have their associated values used for munging
  assertthat::assert_that(length(variable_keys) == length(variable_values))
  
  #TODO: Consider implementing a check of all paramaters
  #https://docs.dhis2.org/en/develop/using-the-api/dhis-core-version-master/data.html
  
  #Requirements 
  # The following constraints apply to the data value sets resource:
  #   
  # At least one data set must be specified.
  # 
  # Either at least one period or a start date and end date must be specified.
  # 
  # At least one organisation unit must be specified.
  # 
  # Organisation units must be within the hierarchy of the organisation units of the authenticated user.
  # 
  # Limit cannot be less than zero.
  
  # /api/dataValueSets.json?dataSet=pBOMPrpg1QX&period=201401&orgUnit=DiszpKrYNg8
  
  if (length(variable_keys) > 0) {
  
  # concatenate and format the keys and values provided for the api call
    variable_k_v_pairs <- mapply(function(x, y) paste0(x, "=", y),
                                 variable_keys, variable_values)
    variable_k_v_pairs <- paste0(variable_k_v_pairs, collapse = "&")

    } else {
    variable_k_v_pairs <- NULL
    }
    
    #Create URL Path
    path=paste0("api/dataValueSets.json?",
         variable_k_v_pairs,
         "&paging=false")

    resp <- api_get(
      path = path,
      d2_session = d2_session,
      retry = retry,
      timeout = timeout
    )
    
    #Create Dataframe from api response
    resp <- as.data.frame(resp$dataValues,stringsAsFactors = FALSE) %>%
      dplyr::rename(data_element = dataElement,
                    org_unit = orgUnit,
                    category_option_combo = categoryOptionCombo,
                    attribute_option_combo = attributeOptionCombo,
                    stored_by = storedBy,
                    last_updated = lastUpdated)

    return(resp)
}