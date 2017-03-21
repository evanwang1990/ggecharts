e_title <- function(title = "", subtitle = "", position = c("top", "center")) {
  title_ <- title.default
  title_$text <- title
  title_$subtext <- subtitle
  position <- parse.position(position)
  title_$left <- position[1]
  title_$top <- position[2]
  structure(
    title_,
    class = c("echart", "title")
  )
}

title.default <- list(
  show = TRUE,
  text = "",
  textStyle = list(
    fontStyle = "normal",
    fontWeight = "bolder",
    fontFamily = "sans-serif",
    fontSize = 18
  ),
  textAlign = "center",
  textBaseline = "middle",
  subtext = "",
  subtextStyle = list(
    fontStyle = "oblique",
    fontWeight = "normal",
    fontFamily = "sans-serif",
    fontSize = 12
  ),
  itemGap = 10,
  left = "center",
  top = "top"
)
