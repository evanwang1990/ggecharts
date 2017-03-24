e_grid <- function(width, height, charts) {
  stopifnot(is.integer(width), is.integer(height), width <= 6, height <= 6, inherits(charts, "chart"))
  charts$grid <-
  charts <- complete_charts(charts)

  invisible(
    structure(
      charts,
      class = c("chartGrid", class(charts))
    )
  )
}


grid.default <- structure(
  list(
    tmp = list(
      width = width,
      height = height
    )
  ),
  class = "chart",
  element = "grid"
)

complete_charts <- function(charts) {
  elements <- c("title", "legend", "xAxis", "yAxis", "tooltip", "series") #TODO: radar and so on has no xAxis
  to_add_elements <- setdiff(elements, names(charts))
  for (ele in to_add_elements) {
    if (ele == "title") {
      charts <- add(charts, e_title())
    } else if (ele == "tooltip") {
      charts <- add(charts, e_tooltip())
    }
  }
  invisible(charts)
}

grid_param <- list(
  list(width = 4, height = 4))

echart_layout <- function(grid_params) {
  unit_width <- unit_height <- 0.13
  gap <- 0.016
  margin <- 0.07
  layout_matrix <- matrix(NA, nrow = 6, ncol = 6)
  for (i in 1:length(grid_params)) {
    layout_matrix <- auto_layout(layout_matrix, grid_params[[i]]$width, grid_params[[i]]$height, i)
  }
  layout_matrix <<- layout_matrix
  res <- vector("list", length(grid_params))
  for (i in 1:length(grid_params)) {
    vertexes <- sapply(range(which(layout_matrix == i)), function(x) parseLocation(x, nrow(layout_matrix)))
    res[[i]] <- list(
      left = percent(margin + (vertexes[2, 1] - 1) * (unit_width + gap)),
      top = percent(margin + (vertexes[1, 1] - 1) * (unit_height + gap)),
      width = percent(unit_width * (diff(vertexes[2,]) + 1) + diff(vertexes[2,]) * gap),
      height = percent(unit_height * (diff(vertexes[1,]) + 1) + diff(vertexes[1,]) * gap)
    )
  }
  res
}

auto_layout <- function(layout_matrix, width, height, order) {
  success <- FALSE
  if ((rn <- nrow(layout_matrix) - height + 1) < 1)
    stop("there's no enough space")
  available_rows <- which(apply(matrix(layout_matrix[1:rn,], nrow = rn), 1, function(x) any(is.na(x))))
  if (length(available_rows) < 0)
    stop("there's no enough space")
  for (ar in available_rows) {
    if ((cn <- ncol(layout_matrix) - width + 1) < 1)
      stop("there's no enough space")
    available_cols <- which(is.na(layout_matrix[ar,1:cn]))
    if (length(available_cols) < 0)
      next
    for (ac in available_cols) {
      available_mat <- layout_matrix[ar:(ar + height - 1), ac:(ac + width - 1)]
      if (all(is.na(layout_matrix[ar:(ar + height - 1), ac:(ac + width - 1)]))) {
        layout_matrix[ar:(ar + height - 1), ac:(ac + width - 1)] <- order
        success <- TRUE
        break
      }
    }
    if (success)
      break
  }
  if (success) {
    return(layout_matrix)
  } else {
    stop("there's no enough space")
  }
}

parseLocation <- function(iloc, rows) {
  r <- iloc %% rows
  c <- iloc %/% rows + 1
  if (r == 0) {
    r <- rows
    c <- c -1
  }
  c(r, c)
}
