runExample <- function(example) {
  # locate all the shiny app examples that exist
  #validExamples <- list.files(system.file("shiny-examples", package = "datimutils"))
  validExamples <- list.files("~/Documents/Repos/datimutils/inst/shiny-examples")
  
  validExamplesMsg <-
    paste0(
      "Valid examples are: '",
      paste(validExamples, collapse = "', '"),
      "'")
  
  # if an invalid example is given, throw an error
  if (missing(example) || !nzchar(example) ||
      !example %in% validExamples) {
    stop(
      'Please run `runExample()` with a valid example app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }
  
  # find and launch the app
  #appDir <- system.file("shiny-examples", example, package = "datimutils")
  appDir <- "~/Documents/Repos/datimutils/inst/shiny-examples/OAuth"
  shiny::runApp(appDir, display.mode = "normal")
}



# • In your package code, use `rlang::is_installed("shiny")` or `rlang::check_installed("shiny")` to test if shiny is installed
# • Then directly refer to functions with `shiny::fun()`