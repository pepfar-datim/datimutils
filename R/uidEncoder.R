##' @title getEncryptPassword.
##' @description Creates and/or returns keyring password for encryption/decryption
##' @return password in keyring
##'
#getEncryptPassword <- function(){
#password <- try(keyring::key_get(
#  service = "EncryptUID",
#))
#if ("try-error" %in% class(password)) {
#  keyring::key_set(service = "EncryptUID")
#  password <- keyring::key_get(
#    service = "EncryptUID"
#  )
#}
#return(password)
#}
#
#
##' @title encryptUIDS
##' @description Encrypts uids in mocks.
##' @param url the url to create the mock from
##' @param first_time specify true here to create the mock and encrypt, if not, have a mock to encrypt already
##'
#encryptUIDS <- function(url, first_time = F){
#  password <- getEncryptPassword()
#  if(first_time){
#    httptest::start_capturing(simplify = FALSE)
#    httr::content(httr::GET(url))
#    httptest::stop_capturing()
#  }
#  response <- dget(paste0(httptest::build_mock_url(url), ".R"))
#  x <- rawToChar(response$content)
#  replacements <- as.character(sample(10000000000:99999999999, stringr::str_count(x,'"[a-zA-Z0-9]{11}\\"')))
#  extracts <- stringr::str_extract_all(x, '"[a-zA-Z0-9]{11}\\"')[[1]]
#  response$content <- stringi::stri_replace_all_fixed(x, pattern = extracts, replacement = replacements, vectorize_all = F)
#  passkey <- sodium::sha256(charToRaw(password))
#  plaintext.raw <- serialize(extracts, NULL)
#  response$encrypted <- sodium::data_encrypt(plaintext.raw, key = passkey)
#  dput(response, paste0(httptest::build_mock_url(url), ".R"))
#}
#
##' @title decryptUIDS
##' @description Decrypts uids in mocks.
##' @param url the url to create the mock from
##'
#decryptUIDS <- function(url){
#  password <- getEncryptPassword()
#  response <- dget(paste0(httptest::build_mock_url(url), ".R"))
#  true_ids <- unserialize(sodium::data_decrypt(response$encrypted, key = sodium::sha256(charToRaw(password))))
#  extracts <- unlist(stringr::str_extract_all(true_ids, '"[a-zA-Z0-9]{11}\\"'))
#  x <- response$content
#  fake_ids <- unlist(stringr::str_extract_all(x, '[0-9]{11}'))
#  x_replace <- stringi::stri_replace_all_regex(x, pattern = fake_ids, replacement = extracts, vectorize_all = F)
#  response$content <- x_replace
#  dput(response, paste0(httptest::build_mock_url(url), ".R"))
#}

#' @title anonimize
#' @description anonimizes a vector
#' @param x the vector to anonimize
#' @param salt_back the salt to add on the end
#' @param salt_front the salt to add on front
#' @param algo the algorithm to use to anonimize
#' @return anonimized vector
anonymize <- function(x, salt_front, salt_back, algo="sha512"){
  hashes <- vapply(paste0(salt_front,x,salt_back), function(object) digest::digest(object, algo=algo), FUN.VALUE="", USE.NAMES=TRUE)
  hashes <- substring(hashes,1,11)
  hashes <- ifelse(grepl("^[0-9]",hashes), paste0(sample(LETTERS, sum(stringr::str_count(hashes,"^[0-9]")), replace = T), substr(hashes,2,11)),  hashes)
  return(hashes)
}

#' @title anonimizeUIDS
#' @description Decrypts uids in mocks.
#' @param url the url to create the mock from or alternatively encrypt an already made one
#' @param salt_back the salt to add on the end
#' @param salt_front the salt to add on front
#' @param first_time if first time is true mock will be created
#'
anonimizeUIDS <- function(url, salt_front, salt_back, exception, first_time = F){
  if(first_time){
    httptest::start_capturing(simplify = FALSE)
    httr::content(httr::GET(url))
    httptest::stop_capturing()
  }
  response <- dget(paste0(httptest::build_mock_url(url), ".R"))
  x <- rawToChar(response$content)
  extracts <- stringr::str_extract_all(x, '(id.{2})("[a-zA-Z0-9]{11}\\")')[[1]]
  extracts <- gsub('id.{3}', "", extracts)
  extracts <- gsub("[^[:alnum:] ]", "", extracts)
  exception_pos <- which(extracts %in% exception)
  replacements <- anonymize(gsub("[^[:alnum:] ]","",extracts), salt_front=salt_front, salt_back=salt_back)
  replacements[exception_pos] <- exception
  put_in <- stringi::stri_replace_all_fixed(x, pattern = extracts, replacement = replacements, vectorize_all = F)
  response$content <- substitute(charToRaw(put_in))
  dput(response, paste0(httptest::build_mock_url(url)))
}