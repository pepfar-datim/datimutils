#' @title getEncryptPassword.
#' @description Creates and/or returns keyring password for encryption/decryption
#' @return password in keyring
#'
getEncryptPassword <- function(){
password <- try(keyring::key_get(
  service = "EncryptUID",
))
if ("try-error" %in% class(password)) {
  keyring::key_set(service = "EncryptUID")
  password <- keyring::key_get(
    service = "EncryptUID"
  )
}
return(password)
}


#' @title encryptUIDS
#' @description Encrypts uids in mocks.
#' @param url the url to create the mock from
#' @param first_time specify true here to create the mock and encrypt, if not, have a mock to encrypt already
#'
encryptUIDS <- function(url, first_time = F){
  password <- getEncryptPassword()
  if(first_time){
    httptest::start_capturing(simplify = FALSE)
    httr::content(httr::GET(url))
    httptest::stop_capturing()
  } else {repsonse <- dget(paste0(httptest::build_mock_url(url), ".R"))}
  response <- dget(paste0(httptest::build_mock_url(url), ".R"))
  x <- rawToChar(response$content)
  replacements <- as.character(sample(10000000000:99999999999, stringr::str_count(x,'"[a-zA-Z0-9]{11}\\"')))
  extracts <- stringr::str_extract_all(x, '"[a-zA-Z0-9]{11}\\"')[[1]]
  response$content <- stringi::stri_replace_all_fixed(x, pattern = extracts, replacement = replacements, vectorize_all = F)
  passkey <- sodium::sha256(charToRaw(password))
  plaintext.raw <- serialize(extracts, NULL)
  response$encrypted <- sodium::data_encrypt(plaintext.raw, key = passkey)
  dput(response, paste0(httptest::build_mock_url(url), ".R"))
}

#' @title decryptUIDS
#' @description Decrypts uids in mocks.
#' @param url the url to create the mock from
#'
decryptUIDS <- function(url){
  password <- getEncryptPassword()
  response <- dget(paste0(httptest::build_mock_url(url), ".R"))
  true_ids <- unserialize(sodium::data_decrypt(response$encrypted, key = sodium::sha256(charToRaw(password))))
  extracts <- unlist(stringr::str_extract_all(true_ids, '"[a-zA-Z0-9]{11}\\"'))
  x <- response$content
  fake_ids <- unlist(stringr::str_extract_all(x, '[0-9]{11}'))
  x_replace <- stringi::stri_replace_all_regex(x, pattern = fake_ids, replacement = extracts, vectorize_all = F)
  response$content <- x_replace
  dput(response, paste0(httptest::build_mock_url(url), ".R"))
}


