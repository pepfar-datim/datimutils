#' @export
#' @title getMyStreams(d2_session = dynGet("d2_default_session", inherits = TRUE))
#' @description Returns datim user streams. For an implementation
#' example see: https://github.com/flopez-bao/shinyapps-datimutils-security-example-usgandpartners
#' @param d2_session the d2Session object, default is "d2_default_session",
getMyStreams <-
  function(d2_session = dynGet("d2_default_session", inherits = TRUE)) {
    # pull all user groups streams
    tryCatch(
      expr = {
        user_groups <- getUserGroups(values = d2_session$me$userGroups$id,
                                     d2_session = d2_session)
      },
      error = function(e) {
        stop("There was an error retrieving the user group information! Make sure you are logged into DATIM.")
      }
    )

    # select from user_groups everything that is a data stream and needed for classification
    user_groups <- user_groups[grepl("Data (.+?) access", user_groups)]

    # remove Data Access strings
    streams <- sort(gsub("Data | access", "", user_groups))

    return(streams)
  }
