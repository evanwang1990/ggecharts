createElmemtID <- function(theme = c("dark", "infographic", "macarons", "roma", "shine", "vintage"), bytes = 10) {
  theme <- match.arg(theme)
  id <- paste(as.hexmode(sample(0:255, bytes, replace = TRUE)), collapse = "")
  paste(c(theme, id), collapse = "-")
}
