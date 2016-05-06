
#' Computes hash for any object.
#' 
#' @importFrom digest digest
hash <- function (obj) {
  digest(obj, algo='md5')
}


#' @importFrom digest digest
crc32 <- function (obj) {
  digest(obj, algo='crc32')
}


#' @importFrom stringi stri_sub
shorten <- function (hash) {
  paste0(stri_sub(hash, 1, 4), stri_sub(hash, -4))
}
