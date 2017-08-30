.onAttach <- function(...) {
  pkgs <- c("curl", "jsonlite", "httr", "xml2", "rvest", "purrr", "dplyr", "stringi", "splashr")
  tmp <- suppressPackageStartupMessages(
    lapply(pkgs, library, character.only = TRUE, warn.conflicts = FALSE)
  )
  invisible()
}
