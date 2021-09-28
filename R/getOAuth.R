library(shiny)
library(httr)

# OAuth setup --------------------------------------------------------

#Prep for local or deployed app

# if (interactive()) {
#   # testing url
#   options(shiny.port = 8100)
#   APP_URL <- "http://localhost:8100/"
# } else {
#   # deployed URL
#   APP_URL <- "https://servername/path-to-app" #This will be your shiny server path
# }

#Documentation 
# Note that secret is not really secret, and it's fine to include inline
# Copy information directly from the dhis2.org web settings
app <- oauth_app("OAuth2 Demo Client", #dhis2 = Name
                 key = "demo",         #dhis2 = Client ID
                 secret = "a58465fdf-d65c-5865-8b23-346c645c9b5", #dhis2 = Client Secret
                 redirect_uri = "http://localhost:8100/" #"http://localhost:1410/"#"http://localhost:8100/"#APP_URL  
)

# Create endpoints with oauth_endpoint()
# Fri Sep 24 16:49:46 2021 ------ This is def where the problem arises
#Documentation link https://httr.r-lib.org/reference/oauth_endpoint.html 
api <- oauth_endpoint(base_url = "https://play.dhis2.org/2.36.3/uaa/oauth",
                      request=NULL,#"requestToken", #Documentation says to leave this NULL for OAuth2 
                      authorize = "authorize",#authorize?client_id=demo&response_type=code&redirect_uri=http://localhost:8100/", #"authorize",
                      access="token/"#"token" This is the correct url not sure why it won't work #token/grant_type=authorization_code",
                      
                      
) 

# twitter = oauth_endpoint(base_url = "https://api.twitter.com/oauth", 
#                                       request = "request_token",
#                                       authorize = "authenticate", 
#                                       access = "access_token")

#This is rather open and can be toned back as deemed necessary
scope <- "ALL"

# Shiny -------------------------------------------------------------------

has_auth_code <- function(params) {
  # params is a list object containing the parsed URL parameters. Return TRUE if
  # based on these parameters, it looks like auth codes are present that we can
  # use to get an access token. If not, it means we need to go through the OAuth
  # flow.
  return(!is.null(params$code))
}

# Your regular UI goes here, for when everything is properly auth'd
ui <- fluidPage(
  verbatimTextOutput("code")
)

# A little-known feature of Shiny is that the UI can be a function, not just
# objects. You can use this to dynamically render the UI based on the request.
# We're going to pass this uiFunc, not ui, to shinyApp(). If you're using
# ui.R/server.R style files, that's fine too--just make this function the last
# expression in your ui.R file.
uiFunc <- function(req) {
  if (!has_auth_code(parseQueryString(req$QUERY_STRING))) { #IF does NOT have token take them to
    url <- oauth2.0_authorize_url(api, app, scope = scope)
    redirect <- sprintf("location.replace(\"%s\");", url)
    tags$script(HTML(redirect))
  } else {
    ui
  }
}

server <- function(input, output, session) {
  
  params <- parseQueryString(isolate(session$clientData$url_search))
  # observe({print(session$clientData$url_search)})
  # print(paste("Parameters equal",params))
  if (!has_auth_code(params)) {
    return()
  }
  
  # Link to Documenation https://httr.r-lib.org/reference/oauth2.0_token.html
  # Manually create a token
  token <- oauth2.0_token(
    app = app,
    endpoint =api#, #"https://play.dhis2.org/2.36.3/uaa/oauth/token/",#api,#paste0("https://play.dhis2.org/2.36.3/uaa/oauth/token/grant_type=authorization_code/code=",params$code), #api,
    #When I commented this out it broke a ton of things, BUT it did let me through???
    # credentials = oauth2.0_access_token(app = app,
    #                                     code = params$code,
    #                                     endpoint = api
    #                                     #user_params = 
    #                                     #redirect_uri = APP_URL
    #                                  )

)
  print(token)
  print(params$code)
  #print(token$credentials)
  print(app$redirect_uri)
  #print(token$doc)
  
  
  
  base_url="play.dhis2.org/2.36.3/"
  #other="uaa/oauth/token/grant_type=authorization_code/"
  # form url
  #url <- utils::URLencode(URL = paste0(base_url,other))
  url <- utils::URLencode(URL = paste0(base_url, "api", "/me"))
  handle <- httr::handle(base_url)
  
  headers = c(
    `Accept` = 'application/json'
  )
  
  data = list(
    `grant_type` = 'authorization_code',
    `code` = params$code
  )
  
  
  resp <- httr::POST(url = 'https://play.dhis2.org/2.36.3/uaa/oauth/token',
                     httr::add_headers(.headers=headers),
                     body = data,
                     httr::authenticate('demo', '9e6c6af3c-36a8-9768-ab8b-875866e4867')
                     )
  
  #Works but returens webpage
  # resp <- httr::POST(url = paste0("https://play.dhis2.org/2.36.3/uaa/oauth/token/grant_type=authorization_code/code=",params$code),
  #                    httr::add_headers(.headers=headers),
  #                    #body = data,
  #                    httr::authenticate('demo', '9e6c6af3c-36a8-9768-ab8b-875866e4867')
  # )
  # 
  
  #WITH PW
  # resp <- httr::POST(url = 'https://play.dhis2.org/2.36.3/uaa/oauth/token%20-d%20grant_type=password%20-d%20username=admin%20-d%20password=district',
  #                   httr::add_headers(.headers=headers),
  #                   httr::authenticate('demo', '9e6c6af3c-36a8-9768-ab8b-875866e4867')
  #                   )
  
  # Logging in here will give us a cookie to reuse
  # resp <- httr::GET(
  #   url,
  #   #httr::authenticate('admin','district'), #this works as expected so the token isn't being validated
  #   #httr::authenticate('Bearer','QFSg1wgD_4bwx4lAxfSpdbqIKYE'),
  #   config(token = token),
  #   httr::timeout(60),
  #   handle = handle
  # )
  
  #GET(url, add_headers(Authorization = paste("Bearer", token)))
  #resp <- GET(url, config(token = token),handle=handle)
  # TODO: check for success/failure here
  
  output$code <- renderText(content(resp, "text"))
  
}

# Note that we're using uiFunc, not ui!
shinyApp(uiFunc, server)




# headers = c(
#   `Authorization` = 'Bearer QFSg1wgD_4bwx4lAxfSpdbqIKYE'
# )
# 
# res <- httr::GET(url = 'https://play.dhis2.org/2.36.3/api/33/dataElements.json', httr::add_headers(.headers=headers))


















# headers = c(
#   `Accept` = 'application/json'
# )
# 
# data = list(
#   `grant_type` = 'authorization_code',
#   `code` = 'XYZ'
# )
# 
# res <- httr::GET(url = 'http://SECRET=9e6c6af3c-36a8-9768-ab8b-875866e4867',
#                  httr::add_headers(.headers=headers),
#                  body = data, httr::authenticate('demo', '$SECRET'))
# 
# 
# 
# 
# # Set Up
# url = "https://canvas.{institution}.edu/api/v1/courses"
# key = "{secret_key}"
# 
# # OPTION 1
# GET(url, add_headers(Authorization = paste("Bearer", key, sep = " ")))
# 
# # OPTION 2
# courses.request = GET(url, add_headers(Authorization = "Bearer {secret_key}"))




# res <- httr::POST(url = 'application/json%20https://play.dhis2.org/dev/uaa/oauth/token%20-d%20grant_type=authorization_code%20-d%20code=XYZ',
#                   httr::authenticate('demo', '9e6c6af3c-36a8-9768-ab8b-875866e4867 -H Accept'))






# Sat Sep 11 10:33:56 2021 ------------------------------
#Easiest way to make the client is by logging into DHIS2 and creating it via settings
#NOTE will have to redone daily since dhis2 pointed to currently is reset nightly

#Step 2 
#Go here
#https://play.dhis2.org/2.36.3/uaa/oauth/authorize?client_id=demo&response_type=code&redirect_uri=http://localhost:8100/




# Thu Sep 16 17:59:25 2021 ------------------------------
#Without Shiny 
# oauth2.0_token(
#   endpoint = oauth_endpoint("requestToken","authorize", "accessToken",
#                             base_url = "https://play.dhis2.org/2.36.3/uaa/oauth"),
#   app = oauth_app("OAuth2 Demo Client",
#                   key = "demo",
#                   secret = "9e6c6af3c-36a8-9768-ab8b-875866e4867"),
#   #redirect_uri = "http://localhost:1410/"),
#   scope = c("ALL"),
#   cache = FALSE
# )





# oauth2.0_token(
#   endpoint = oauth_endpoint(NULL,"authorize?client_id=demo&response_type=code", "token/grant_type=authorization_code",
#                             base_url = "https://play.dhis2.org/2.36.3/uaa/oauth"),
#   app = oauth_app("OAuth2 Demo Client",
#                   key = "demo",
#                   secret = "9e6c6af3c-36a8-9768-ab8b-875866e4867",
#                   redirect_uri = "http://localhost:8100/"),
#   scope = c("ALL"),
#   cache = FALSE
# )






