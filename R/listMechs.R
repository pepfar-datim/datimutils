#' @export
#' @title listMechs(option_fields = c("name", "id", "code"),
#' combo_fields = "id",
#' d2_session = dynGet("d2_default_session",inherits = TRUE)
#' @description Returns a dataframe with all mechanisms a user has access to. For an implementation
#' example see: https://github.com/flopez-bao/shinyapps-datimutils-security-example-usgandpartners
#' @param option_fields fields passed to the getMetaData endpoint.
#' @param combo_fields fields passed to getCatOptionCombos call.
#' @param d2_session the d2Session object, default is "d2_default_session"
#'
listMechs <- function(option_fields = c("name", "id", "code"),
                        combo_fields = "id",
                        d2_session = dynGet("d2_default_session",
                                            inherits = TRUE)) {


  if (isFALSE(setequal(option_fields, c("name", "id", "code")))) {
    stop("other fields are currently not supported.")
  } else if (combo_fields != "id") {
    stop("other fields are currently not supported.")
  } else {

    # Pull category options
    df <- getMetadata(categoryOptions,
                      categories.id %.in% "SH885jaRe0o",
                      fields = option_fields,
                      d2_session = d2_session)

    # Adorn with category option combos
    df["combo_id"] <- datimutils::getCatOptionCombos(df$code,
                                                     by = code,
                                                     fields = combo_fields,
                                                     d2_session = d2_session)

    # Rename columns
    names(df)[names(df) == "id"] <- "option_id"
    names(df)[names(df) == "code"] <- "mech_code"

    # Select columns for export
    df <- df[, c("mech_code", "name", "option_id", "combo_id")]

    return(df)
  }

}
