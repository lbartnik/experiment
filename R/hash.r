
#' @importFrom digest digest
hash <- function (obj) {
  digest(obj, algo='crc32')
}
