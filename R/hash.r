
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
#' @export
shorten <- function (hash) {
  stri_sub(hash, 1, 8)
}
