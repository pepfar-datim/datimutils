#' @export
#' @title convertconvertFYQToCQ
#' @description converts fiscal year and quarter to calendar year and quarter
#' @param fiscal_yyyy_int a 4 digit number
#' @param fiscal_q_int a single number
#' @return "yyyyQQ"
#' @examples convertFYQToCQ(1995,1)
convertFYQToCQ <- function(fiscal_yyyy_int, fiscal_q_int) {
  fiscal_yyyy_int <- as.integer(fiscal_yyyy_int)
  fiscal_q_int <- as.integer(fiscal_q_int)
  if (!(fiscal_q_int %in% c(1, 2, 3, 4))) {
    stop("Invalid fiscal quarter")
    }
  if (fiscal_q_int == 1) {
    calendar_q_int  <-  4
    calendar_yyyy_int <- fiscal_yyyy_int - 1
  } else {
    calendar_q_int <- fiscal_q_int - 1
    calendar_yyyy_int <- fiscal_yyyy_int
  }
  return(paste0(calendar_yyyy_int, "Q", calendar_q_int))
}
