print.chartGrid <- function(grids) {
  if (!inherits(grids, "chartGrids"))
    grids <- list(grids)
  layout_params <- lapply(grids, function(g) g[["grid"]])
  layouts <- echart_layout(layout_params)
  grids
}
