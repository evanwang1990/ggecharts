e_tooltip <- function(show = TRUE, trigger = c("item", "axis"), enterable = FALSE) {
  tooltip_ <- tooltip.default
  tooltip_$show <- show
  trigger <- match.arg(trigger)
  tooltip_$trigger <- trigger
  tooltip_$enterable <- enterable
  invisible(
    structure(
      tooltip_,
      class = "echart",
      element = "tooltip"
    )
  )
}

tooltip.default <- list(
  show = TRUE,
  trigger = "item",
  enterable = FALSE,
  confine = TRUE
)
