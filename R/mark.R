# tomark: "minX", "maxY", "average"
e_markPoint <- function(tomark, symbol = c("pin", "circle", "rect", "roundRect", "triangle", "diamond", "arrow"),
                       symbol.size = 50, label.show = TRUE,
                       label.position = c("inside", "center", "middle"),
                       each.color = FALSE) {
  if (!is.function(tomark)) {
    stopifnot(all(tomark %in% c("minX", "maxX", "averageX", "minY", "maxY", "averageY")))
  }
  markPoint <- markPoint.default
  #tomark
  symbol <- match.arg(symbol, several.ok = FALSE)
  markPoint$symbol <- symbol
  markPoint$symbolSize <- symbol.size
  markPoint$label$normal$show <- label.show
  markPoint$label$normal$position <- parsePosition4Point(label.position)

  invisible(
    structure(
      markPoint,
      class = "echart",
      element = "mark",
      tomark = tomark
    )
  )
}

parsePosition4Point <- function(position) {
  inside <- ""
  if ("inside" %in% position) {
    inside <- "inside"
    position <- setdiff(position, "inside")
  }
  position <- parsePosition(position)
  position_ <- which(!position %in% c("auto", "center", "middle"))
  if (inside == "") {
    res <- ifelse(length(position_) == 0, "top", position[1])
  } else {
    res <- paste(c(inside, ifelse(2 %in% position_, cap(position[2]), ""), ifelse(1 %in% position_, cap(position[1]), "")), collapse = "")
  }
  res
}


markPoint.default <- list(
  symbol = "pin",
  symbolSize = 50,
  label = list(
    normal = list(
      show = TRUE,
      position = "inside"
    )
  )
)
