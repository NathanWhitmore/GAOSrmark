# wrapper for bind_rows()
# turns multiple files into single data frame for comparison

amalgamate <- function (..., .id = NULL) {
  suppressWarnings(bind_rows(...))

}
