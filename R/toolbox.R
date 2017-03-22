e_toolbox <- function(show = TRUE, position = c("top", "right"), box = c("saveAsImage", "restore", "dataView", "dataZoom", "magicType"), magicType = c("line", "bar", "stack", 'tiled')) {
  toolbox_ <- toolbox.default
  if (!show) {
    toolbox_$show <- FALSE
  } else {
    position <- parsePosition(position)
    toolbox_$left = position[1]
    toolbox_$top <- position[2]
    box <- match.arg(box, several.ok = TRUE)
    for (box_ in setdiff(c("saveAsImage", "restore", "dataView", "dataZoom", "magicType"), box)) {
      toolbox_$feature[[box_]]$show = FALSE
    }
    if ("magicType" %in% box) {
      magicType <- match.arg(magicType, several.ok = TRUE)
      if (length(magicType) > 0)
        toolbox_$feature$magicType$type <- magicType
    }
  }

  invisible(
    structure(
      toolbox_,
      class = "chart",
      element = "toolbox"
    )
  )
}

toolbox.default <- list(
  show = TRUE,
  orient = "horizontal",
  itemSize = 15,
  left = "auto",
  top = "auto",
  feature = list(
    saveAsImage = list(
      show = TRUE,
      name = htmlwidgets::JS(saveImageName)
    ),
    restore = list(
      show = TRUE
    ),
    dataView = list(
      show = TRUE
    ),
    dataZoom = list(
      show = TRUE,
      xAxisIndex = 0,
      yAxisIndex = 0
    ),
    magicType = list(
      show = TRUE,
      type = ""
      # option
      # seriesIndex
    )
  )
)

saveImageName <- "
function() {
var image_name = 'test';
var today = new Date();
var year= today.getFullYear();
var month = today.getMonth();
month = month < 10 ? ('0' + month) : month;
var date = today.getDate();
date = date < 10 ? ('0' + date) : date;
image_name += '_' + year + month + date + '_';
for(var i = 0; i < 8; i++) {
image_name += Math.round(Math.random() * 255).toString(16);
}
return image_name;
}()
"
