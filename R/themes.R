#' Dark theme for ggplot2
#' @name dark_theme
#' @description Implement a dark ggplot2 theme
#'
#' @param base_font a string indicating which font to use (default is Fira
#'        Sans Condensed)
#' @param title_font a string indicating which font to use (default is Fira
#'        Sans Condensed Medium)
#' @param base_size a numerica value indicating the base font size of the plot
#'
#' @return a predefined \code{ggplot2} theme based on
#' \code{dark_theme_minimal()} from \code{ggdark}
#'
#' @examples
#' ggplot(sample_data) +
#'   geom_bar(aes(x = var5)) +
#'   dark_theme()
#'
#' @import ggplot2 extrafont
#'
#' @export
dark_theme <- function(base_font = "Fira Sans Condensed",
                       title_font = "Fira Sans Condensed Medium",
                       base_size = 12){
  ddpcr::quiet(
  th <- ggdark::dark_theme_minimal(
    base_family = title_font,
    base_size = base_size
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(family = title_font),
      plot.background = ggplot2::element_rect(fill = "grey10"),
      panel.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "grey30", size = 0.2),
      panel.grid.minor = ggplot2::element_line(color = "grey30", size = 0.1),
      legend.background = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
      ),
  all = TRUE
  )
  ddpcr::quiet(ggdark::invert_geom_defaults(), all = TRUE)
  return(th)
}

#' Light theme for ggplot2
#' @name light_theme
#' @description Implement a light ggplot2 theme
#'
#' @inheritParams dark_theme
#'
#' @return a predefined \code{ggplot2} theme based on \code{theme_minimal()}
#'
#' @examples
#' ggplot(sample_data) +
#'   geom_bar(aes(x = var5)) +
#'   light_theme()
#'
#' @import ggplot2 extrafont
#'
#' @export
light_theme <- function(base_font = "Fira Sans Condensed",
                        title_font = "Fira Sans Condensed Medium",
                        base_size = 12){
  th <- ggplot2::theme_minimal(
    base_family = title_font,
    base_size = base_size
  ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(family = title_font),
      panel.background = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
      )
  return(th)
}


#' Dark theme for ggplot2
#' @name dark_map_theme
#' @description Implement a dark ggplot2 theme
#'
#' @inheritParams dark_theme
#'
#' @return a predefined \code{ggplot2} theme based on
#' \code{dark_theme_minimal()} from \code{ggdark}
#'
#' @examples
#' ggplot(sample_data) +
#'   geom_bar(aes(x = var5)) +
#'   dark_theme()
#'
#' @import ggplot2 extrafont
#'
#' @export
dark_map_theme <- function(base_font = "Fira Sans Condensed",
                           title_font = "Fira Sans Condensed Medium",
                           base_size = 12){
  ddpcr::quiet(
    th <- ggdark::dark_theme_minimal(
      base_family = title_font,
      base_size = base_size
    ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(family = title_font),
        plot.background = ggplot2::element_rect(fill = "grey10"),
        panel.background = ggplot2::element_blank(),
        panel.grid = element_line(color = "transparent"),
        legend.background = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.text = element_blank()
      ),
    all = TRUE
  )
  ddpcr::quiet(ggdark::invert_geom_defaults(), all = TRUE)
  return(th)
}

#' Light theme for ggplot2
#' @name light_map_theme
#' @description Implement a light ggplot2 theme
#'
#' @inheritParams dark_theme
#'
#' @return a predefined \code{ggplot2} theme based on \code{theme_minimal()}
#'
#' @examples
#' ggplot(sample_data) +
#'   geom_bar(aes(x = var5)) +
#'   light_theme()
#'
#' @import ggplot2 extrafont
#'
#' @export
light_map_theme <- function(base_font = "Fira Sans Condensed",
                            title_font = "Fira Sans Condensed Medium",
                            base_size = 12){
  th <- ggplot2::theme_minimal(
    base_family = title_font,
    base_size = base_size
  ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(family = title_font),
      panel.background = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = element_blank(),
      panel.grid.major = element_line(color = "transparent")
    )
  return(th)
}
