#' @title Theme for orthographic plots in \code{ggplot2}
#' @description This is a set up of a theme for \code{ggplot2}, specifically for
#' orthographic plots
#'
#' @return A \code{ggplot2} theme
#' @export
#'
#' @examples
#' theme_ortho()
#' @importFrom ggplot2 theme element_rect element_blank
#' @importFrom ggplot2 unit margin element_text
theme_ortho = function() {
  x = theme(
    panel.background = element_rect(fill = "black"),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "black"),
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    strip.text.y = element_blank(),
    # strip.text.x = element_text(size = 0),
    # strip.text.y = element_text(size = 0),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0, "null"),
    # axis.text = element_text(
    #   margin = unit(0, "null")
    # ),
    axis.text = element_blank(),
    axis.line = element_blank(),
    ##################
    # from SO 31254533
    #################
    plot.margin = unit(c(0,0,0,0), "null"),
    panel.spacing = unit(0, "null"),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0,
                           unit = "null")
  )
  x = x +
    theme(legend.position = c(0.75, 0.25),
          legend.text = element_text(colour = "white"),
          legend.title = element_text(colour = "white"))
  x
}
