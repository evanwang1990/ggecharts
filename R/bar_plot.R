e_bar <- function(data = NULL, x = NULL, y = NULL, color = NULL, stack = NULL) {
  x.name <- deparse(substitute(x))
  y.name <- deparse(substitute(y))
  color.name <- deparse(substitute(color))

  if (x.name == "NULL")
    stop("x is missing")
  if (y.name == "NULL")
    stop("y is missing")


  x <- Eval(x.name, data)
  stopifnot(is.factor(x) || is.character(x))
  if (is.factor(x))
    x <- as.character(x)
  y <- Eval(y.name, data)


  if (color.name != "NULL") {
    color <- Eval(color.name, data)
    if (!is.factor(color))
      color <- factor(color)
  } else {
    tmp_color <- paste0("bar-", randomID(2))
    color <- factor(rep(tmp_color, length(y)))
  }
  tmp_df <- data.frame(x = x, y = y, color = color)
  tmp_tb <- xtabs(y ~ x + color, tmp_df[complete.cases(tmp_df),])

  series.bar <- series.bar.default

  # stack
  if (isTRUE(stack)) {
    stack <- randomID(4, "stack")
  }
  series.bar$stack <- stack

  # bar name
  bar.name <- colnames(tmp_tb)

  series <- rep(list(series.bar), ncol(tmp_tb))
  for (i in 1:ncol(tmp_tb)) {
    series[[i]]$name <- bar.name[i]
    series[[i]]$data <- unname(tmp_tb[,i])
  }

  # axis
  xAxis <- xAxis.default
  xAxis$data <- rownames(tmp_tb)

  yAxis <- yAxis.default

  structure(
    list(
      xAxis = xAxis,
      yAxis = yAxis,
      series = series
    ),
    class = c("chart"),
    element = c("barplot"),
    main = TRUE
  )

}


series.bar.default <- list(
   type = "bar",
   name = "",
   xAxisIndex = 0,
   yAxisIndex = 0,
   stack = NULL,
   # barWidth
   barGap = "30%",
   data = list()
 )
