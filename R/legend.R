e_legend <- function(show = TRUE, position = c("center", "bottom")) {
  legend_ <- legend.default
  position <- parsePosition(position)
  legend_$left <- position[1]
  legend_$top <- position[2]
  if (legend_$top == "middle" || legend_$left == "right") {
    legend_$orient <- "vertical"
  }
  invisible(
    structure(
      legend_,
      class = "echart",
      element = "legend"
    )
  )
}


legend.default <- list(
  show = TRUE,
  left = "auto",
  top = "auto",
  orient = "horizontal",
  selectdMode = TRUE
  # data = list()

)
