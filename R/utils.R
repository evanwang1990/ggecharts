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

parsePosition <- function(position) {
  left <- c("left", "center", "right")
  top <- c("top", "middle", "bottom")
  left_ <- position[position %in% left]
  if (length(left_) == 0)
    left_ <- "auto"
  else if (length(left_) > 1)
    left_ <- left_[1]
  top_ <- position[position %in% top]
  if (length(top_) == 0)
    top_ <- "auto"
  else if (length(top_) > 1)
    top_ <- top_[1]
  c(left_, top_)
}


updateList <- function(base, update) {
  if (is.null(base))
    return(update)

  ele_base <- names(base)
  for (ele in names(update)) {
    if (!ele %in% ele_base || !is.list(base[[ele]])) {
      base[[ele]] <- update[[ele]]
    } else {
      base[[ele]] <- updateList(base[[ele]], update[[ele]])
    }
  }
  base
}


cap <- function(s) {
  paste(toupper(substring(s, 1, 1)),tolower(substring(s, 2)), sep = "", collapse = "")
}
