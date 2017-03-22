add <- function(...) {
  UseMethod("add")
}

add.chart <- function(e1, e2) {
  if (!attr(e1, "main")) {
    stop("LH of `+` is not a main echart object which contains no data")
  }
  element <- attr(e2, "element")
  if (!element %in% names(e1)) {
    e1[[element]] <- e2
  }
  e1[[element]] <- updateList(e1[[element]], e2)

  if (element == "legend") {
    legend_data <- sapply(e1$series, `[[`, "name")
    if (length(legend_data) > 1)
      e1$legend$data <- legend_data
  }

  invisible(structure(
    e1,
    elements = c(attr(e1, "element"), element)
  ))
}

`+.chart` <- add.chart

add_markPoint <- function(e1, e2) {

}
