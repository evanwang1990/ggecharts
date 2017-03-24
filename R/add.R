add <- function(...) {
  UseMethod("add")
}

add.chart <- function(e1, e2) {
  if (!attr(e1, "main")) {
    stop("LH of `+` is not a main echart object which contains no data")
  }
  element <- attr(e2, "element")
  if (!grepl("mark", element)) {
    if (!element %in% names(e1)) {
      e1[[element]] <- e2
    }
    e1[[element]] <- updateList(e1[[element]], e2)
  }

  if (element == "legend") {
    legend_data <- sapply(e1$series, `[[`, "name")
    if (length(legend_data) > 1)
      e1$legend$data <- legend_data
  } else if (element == "markPoint") {
    e1$series <- add_markPoint(e1$series, e2)
  }

  invisible(structure(
    e1,
    elements = c(attr(e1, "element"), element)
  ))
}

`+.chart` <- add.chart

add_markPoint <- function(s, e2) {
  if (!attr(e2, "isfunction")) {
    s <- lapply(s, function(s_) { s_$markPoint <- e2; s_})
  }
  s
}


add.chartGrid <- function(g1, g2) {
  ele_g1 <- names(g1)
  ele_g2 <- names(g2)
  g1 <- c(g1, g2[setdiff(ele_g2, ele_g1)])
  for (ele_ in intersect(ele_g2, ele_g1)) {
    g1[[ele_]] <- combine(g1[[ele_]], g2[[ele_]])
  }
  invisible(
    structure(
      g1
      class = union("chartGrids", class(g1))
    )
  )
}
