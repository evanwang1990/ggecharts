Eval <- function(expr, data = NULL) {
  expr <- parse(text = expr)
  if(is.null(data) || missing(data)) {
    vec <- eval(expr)
  } else {
    e <- evalq(environment(), data, parent.frame())
    vec <- eval(expr, e)
  }

  vec
}

randomID <- function(n, prefix = "") {
  paste(as.hexmode(sample(0:255, n, replace = TRUE)), collapse = "")
}

createElmemtID <- function(theme = c("dark", "infographic", "macarons", "roma", "shine", "vintage"), bytes = 10) {
  theme <- match.arg(theme)
  id <- randomID(bytes)
  paste(c(theme, id), collapse = "-")
}

parse.position <- function(position) {
  left <- c("left", "center", "right")
  top <- c("top", "middle", "bottom")
  left_ <- position[position %in% left]
  if (length(left_) == 0)
    left_ <- "center"
  else if (length(left_) > 1)
    left_ <- left_[1]
  top_ <- position[position %in% top]
  if (length(top_) == 0)
    top_ <- "top"
  else if (length(top_) > 1)
    top_ <- top_[1]
  c(left_, top_)
}
