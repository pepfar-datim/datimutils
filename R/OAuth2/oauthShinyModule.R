#' Allows the easy retreival of a token for using apis in your shiny apps.
#' @param id the id you will use to keep track of this component in your app
#' @return A button that a used presses to login to your given authentication service.
#' @export
#' @examples
#' shinydrawrUI('myrecorder')

shinyOAuthUI <- function(id) {
  ns <- NS(id)
  
  #Grab the external javascript and css
  OAuthrjs <- .get_script("OAuthr.js", "js")
  shinyOAuthjs <- .get_script("shinyOAuth.js", "js")
  
  tagList(
    singleton(
      tags$head( #load external scripts.
        tags$script(HTML(OAuthrjs)),
        tags$script(HTML(shinyOAuthjs))
      )
    ),
    tags$button(id = ns("OAuthButton"), "Login Button")
    
  ) #end tag list.
}


#' Server side component. You supply this with your api credentials.
#'
#' @param input you can ignore this as it is taken care of by shiny
#' @param output you can ignore this as it is taken care of by shiny
#' @param session you can ignore this as it is taken care of by shiny
#' @param api_info A named list of the various api values you need to authenticate. "key", "id", "secret", "url", "token_url" 'https://www.fitbit.com/oauth2/authorize'
#' @param api_key your apps personal api key.
#' @param scope a vector of what you're requesting access to in the API. See the given api docs for examples.
#' @export

shinyOAuth <- function(input, output, session,
                      api_info,
                      response_type = "code"){
  
  key_secret_code <- .to_base_64(paste0(api_info$key, ":", api_info$secret))
  
  #Send over a message to javascript.
  observe({ session$sendCustomMessage(
    type = "initialize_button",
    message = list(
      dom_target = session$ns("OAuthButton"),
      main_url = api_info$OAuth_url,
      api_key = api_info$key,
      scope = api_info$scope,
      response_type = response_type,
      id  = session$ns(""))
  )
  })
  
  # The user's api token in string format.
  result <- reactive({
    
    token_request <- getToken(
      auth_code = input$code ,
      redirect_uri = api_info$redirect_uri,
      key = api_info$key,
      token_url = api_info$token_url,
      key_secret_code = key_secret_code
    )
    
    
    token_request$access_token
  })
  
  
  return(result)
}