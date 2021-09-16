
stop0 <- function(...) {
  stop(..., call. = FALSE)
}

check_frequency_detection <- function(x) {
  if (NROW(x) < 2) {
    stop0(
      "need at least two observations for frequency detection"
    )
  }
}
