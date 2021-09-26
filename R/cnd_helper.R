#' Print Character Vector to Messages
#'
#'
#' @param x A character vector
#' @param sep Separator between each character elements
#' @param prefix Prefix of each character elements
#' @param suffix Suffix of each character elements
#'
#' @return A message
#'
print_messages <- function(x, sep = "\n", prefix = "", suffix = "") {

  pad_len <- length(x) - 1

  # Add optional prefix or suffix
  if (prefix != "" || suffix != "") {
    for (i in seq_along(x)) {
      x[i] <- paste0(prefix, x[i], suffix)
    }
  }
  # Add Separator
  for (i in 1:pad_len) {
    x[i] <- paste0(x[i], .sep = sep)
  }

  message(x)
}
