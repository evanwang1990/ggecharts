e_title <- function(title = "", subtitle = "", position = c("auto", "auto")) {
  title_ <- title.default
  title_$text <- title
  title_$subtext <- subtitle
  position <- parsePosition(position)
  title_$left <- position[1]
  title_$top <- position[2]
  if (title_$left != "auto")
    title_$textAlign <- title_$left
  if (title_$top != "auto")
    title_$textBaseline <- title_$top

  structure(
    title_,
    class = c("echart"),
    element = "title"
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
  textAlign = "",
  textBaseline = "",
  subtext = "",
  subtextStyle = list(
    fontStyle = "oblique",
    fontWeight = "normal",
    fontFamily = "sans-serif",
    fontSize = 12
  ),
  itemGap = 10,
  left = "auto",
  top = "auto"
)
