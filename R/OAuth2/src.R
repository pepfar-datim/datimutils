#In order to load the javascript and css we can first load it to a character var
.get_script <- function(title, type){
  fileName <- system.file(type, title, package = "shinyOAuth")
  readChar(fileName, file.info(fileName))
}

#convert a string to base64
.to_base_64 <- function(string) base64enc::base64encode(charToRaw(string))