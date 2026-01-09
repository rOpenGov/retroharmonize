#' @importFrom assertthat assert_that
#' @importFrom fs is_file
#' @keywords  internal
valid_file_info <- function(file) {
  assertthat::assert_that(
    fs::is_file(file),
    msg = paste0("file='", file, "' is not a file. ")
  )

  file.info(file)
}
