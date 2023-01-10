extrafont::loadfonts()

create_theme_template <- function(...) {
  ggplot2::theme_minimal(...) %+replace%
    ggplot2::theme(
      line = ggplot2::element_line(colour = "black"),
      text = ggplot2::element_text(
        colour = "black",
        family = "Segoe UI Light"
      ),
      axis.title.y = ggplot2::element_text(
        angle = 90,
        margin = ggplot2::margin(0, 20, 0, 5)
      ),
      axis.title.y.right = ggplot2::element_text(
        angle = 90,
        margin = ggplot2::margin(0, 5, 0, 20)
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.margin = ggplot2::margin(5, 5, 5, 5),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "black")
    )
}

nice_colors <- c(
  `navy`           = "#092B57",
  `marine`         = "#4CADB0",
  `wattle`         = "#FCB76C",
  `moss`           = "#778D62",
  `eucalyptus`     = "#DEE9BB",
  `morning glory`  = "#A0C5DD",
  `pindan`         = "#B75B53",
  `galah`          = "#F79E89",
  `terracotta`     = "#F26337",
  `black`          = "#000000",
  `abbey`          = "#58595b",
  `iron`           = "#d1d3d4",
  `white`          = "#ffffff",
  `tamarillo`      = "#A9111F"
)
