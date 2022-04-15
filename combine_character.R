"+" <- function(e1, e2) {
  if (is.character(e1) & is.character(e2)) {
    paste(e1, e2, sep = "")
  } else {
    base::"+"(e1, e2)
  }
}
