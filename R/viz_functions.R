#' @title data visualization
#' @name plot_na
#'
#' @description \code{plot_na} uses dplyr and ggplot2 to return a stacked
#'              barplot showing the counts of NAs for each variable in the
#'              dataset.
#'
#' @param df a data.frame object
#' @param varcut a categorical variable to facet the plot.
#' @param title a string defining the plot title
#' @param theme a string defining theme: "light" (default) or "dark"
#' @param colors a character vector of lenght 2, indicating NA and non-NA
#'        colors in the plot
#'
#' @details This function is tidyverse based. It transforms the data in a
#'          \code{\link{tibble}}, \code{\link{gather}} and \code{\link{count}}
#'          NAs for each variable. Results are ploted in a stacked bar plot
#'          with \code{\link{geom_bar}}. Users can define title, colors, and
#'          can plot results grouped by one variable. In this case the plot
#'          will be faceted with \code{\link{facet_wrap}} horizontally
#'          displayed.
#'
#'          * \code{mode} options: "light" (default), or "dark"
#'
#' @return If \code{varcut} is not passed, then the function plot a simple
#'         stacked barplot showing the counts of NAs for each variable in
#'         the dataset. If is, then the counts are faceted.
#'
#' @examples
#' # plot_na without varcut
#' plot_na(sample_data)
#'
#' # plot_na with varcut (useful for time series data)
#' plot_na(sample_data, varcut = "year")
#'
#' # plot_na manipulating other parameters
#' plot_na(
#'   sample_data,
#'   varcut = "year",
#'   title = "Counts of NAs",
#'   theme = "dark",
#'   colors = c("red", "darkgreen")
#'   )
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @import ggplot2
#'
#' @export
plot_na <- function(df,
                    varcut = NULL,
                    title = NULL,
                    theme = "light",
                    colors = c("darkgrey", "red"),
                    base_family = "Fira Sans Condensed",
                    family = "Fira Sans Condensed Medium") {
  dt <- df %>% tibble::as_tibble() %>% dplyr::mutate_all(as.character)
  if (is.null(varcut)){
    dt <- dt %>%
      tidyr::gather() %>%
      dplyr::mutate(key = factor(.data$key, levels = rev(names(df)))) %>%
      dplyr::group_by(.data$key) %>%
      dplyr::count(na = is.na(.data$value)) %>%
      dplyr::mutate(na = dplyr::if_else(.data$na == TRUE, "Sim", "Nao"))
  } else {
    dt <- dt %>%
      dplyr::rename("varcut" = varcut) %>%
      tidyr::gather("key", "value", -.data$varcut) %>%
      dplyr::mutate(key = factor(.data$key, levels = rev(names(df)))) %>%
      dplyr::group_by(.data$varcut, .data$key) %>%
      dplyr::count(na = is.na(.data$value)) %>%
      dplyr::mutate(na = dplyr::if_else(.data$na == TRUE, "Sim", "Nao"))
  }
  p <- ggplot2::ggplot(dt, ggplot2::aes(
    x = .data$key, y = .data$n, fill = forcats::fct_rev(.data$na)
    )) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_fill_manual(values = colors, "Valor NA?") +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    ggplot2::coord_flip()
  if (!is.null(varcut)) {
    date_cut <- rlang::quo(varcut)
    p <- p + ggplot2::facet_wrap(
      ~varcut, scales = "free_x" ,
      ncol = df %>% dplyr::pull(!!date_cut) %>% unique %>% length
    )
  }
  if (theme == "light") {
    p <- p  + light_theme(base_family, family) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        legend.position = "top"
        )
  } else if (theme == "dark") {
    ddpcr::quiet(
      p <- p + dark_theme(base_family, family) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_blank(),
          legend.position = "top"
          ),
      all = TRUE
      )
    ggdark::invert_geom_defaults()
  }
  return(p)
}

#' @title Bar plot for univariate analysis
#' @name plot_bar
#'
#' @description It counts a categorical variable and plots a publication
#'              ready ggplot2 barplot
#'
#' @param df a data frame
#' @param x a string indicating a categorical variable
#' @param title a string defining the plot title
#' @param fillc bar colors when fill == FALSE
#' @param fill a string indicating a categorical variable
#' @param flip a logical TRUE or FALSE
#' @param theme a string defining theme: "light" (default) or "dark"
#' @param mode a string defining mode: "count" (default) od "prop"
#'
#' @details Uses dplyr and ggplot
#'
#' @return Um grafico de barras para a variavel \code{x} da base \code{df}.
#'
#' @author Bruno Pinheiro
#'
#' @examples
#' # simple barplot
#' plot_bar(sample_data, var5)
#'
#' # plot more than one variable at once
#' vars <- c("var5", "cut_var")
#' plots <- lapply(vars, function(i) plot_bar(sample_data, i))
#'
#' @importFrom rlang .data
#' @import ggplot2
#'
#' @export
plot_bar <- function(df,
                     x,
                     title = NULL,
                     theme = "light",
                     mode = "count",
                     fill = FALSE,
                     flip = FALSE,
                     fillc = "darkgrey",
                     base_family = "Fira Sans Condensed",
                     family = "Fira Sans Condensed Medium") {

  dt <- df %>% tibble::as_tibble() %>% dplyr::rename("target" = x)
  # count variable
  if (mode == "count") {
    counts <-  dt %>%
      dplyr::count(.data$target)
  } else if (mode == "prop") {
    counts <-  dt %>%
      dplyr::count(.data$target) %>%
      dplyr::mutate(n = prop.table(.data$n))
  }
  # identify higher number
  max_num <- max(counts$n)
  # create ggplot2 mapping to geometries
  p <- ggplot2::ggplot(counts, ggplot2::aes(y = .data$n, x = .data$target))
  if (fill == FALSE) {
     p <- p + ggplot2::geom_bar(stat = "identity", fill = fillc)
  } else if (fill == TRUE) {
    p <- p + ggplot2::geom_bar(stat = "identity",
                               ggplot2::aes(fill = .data$target)) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = x))
  }
  # add scales definitions and labels
  if (mode == "count") {
    if (flip == FALSE) {
    p <- p +
      ggplot2::scale_y_continuous(
        limits = c(0, max_num + (max_num / 20))
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = .data$n),
        vjust = -1, fontface = "bold", color = "grey"
      )
    } else if (flip == TRUE) {
      p <- p +
        ggplot2::scale_y_continuous(
          limits = c(0, max_num + (max_num / 20))
        ) +
        ggplot2::geom_text(
          ggplot2::aes(label = .data$n),
          vjust = .5, hjust = -.2, fontface = "bold", color = "grey"
        ) + ggplot2::coord_flip()
      }
  } else if (mode == "prop") {
    if (flip == FALSE) {
      p <- p +
        ggplot2::scale_y_continuous(
          limits = c(0, max_num + (max_num / 20)),
          labels = scales::percent
          ) +
        ggplot2::geom_text(
          ggplot2::aes(label = scales::percent(.data$n, accuracy = .1)),
          vjust = -1, fontface = "bold", color = "grey"
          )
    } else if (flip == TRUE) {
        p <- p +
          ggplot2::scale_y_continuous(
            limits = c(0, max_num + (max_num / 20)),
            labels = scales::percent
            ) +
          ggplot2::geom_text(
            ggplot2::aes(label = scales::percent(.data$n, accuracy = .1)),
            vjust = .5, hjust = -.2, fontface = "bold", color = "grey"
            ) + ggplot2::coord_flip()
    }
  }
  # add theme
  if (theme == "light") {
    p <- p + light_theme(base_family, family)

  } else if (theme == "dark") {
    ddpcr::quiet(p <- p + dark_theme(base_family, family), all = TRUE)
    ggdark::invert_geom_defaults()
  }
  # add labs
  p <- p + ggplot2::labs(
    title = title,
    x = NULL, y = NULL
    )
  # output
  return(p)
}

#' @title Bar plot for univariate analysis
#' @name plot_bar_id
#'
#' @description It counts a categorical variable and plots a publication
#'              ready ggplot2 barplot
#'
#' @param df a data frame
#' @param x a string indicating a categorical variable
#' @param y a string indicating a numeric variable
#' @param title a string defining the plot title
#' @param fillc bar colors when fill == FALSE
#' @param fill a string indicating a categorical variable
#' @param flip a logical TRUE or FALSE
#' @param theme a string defining theme: "light" (default) or "dark"
#' @param mode a string defining mode: "count" (default) or "prop"
#'
#' @details Uses dplyr and ggplot
#'
#' @return Um grafico de barras para a variavel \code{x} da base \code{df}.
#'
#' @author Bruno Pinheiro
#'
#' @examples
#' sample_data %>%
#'   filter(!is.na(var5)) %>%
#'   count(var5) %>%
#'   plot_bar_id(x = "var5",
#'               y = "n",
#'               fill = T)
#'
#' @importFrom rlang .data
#' @import ggplot2
#'
#' @export
plot_bar_id <- function(df,
                        x,
                        y,
                        order = FALSE,
                        theme = "light",
                        mode = "count",
                        fill = FALSE,
                        flip = FALSE,
                        fillc = "darkgrey",
                        base_family = "Fira Sans Condensed",
                        family = "Fira Sans Condensed Medium",
                        title = NULL,
                        subtitle = NULL) {

  dt <- df %>%
    tibble::as_tibble() %>%
    dplyr::rename("targetx" = x, "targety" = y)
  # identify higher number
  max_num <- max(dt$targety)
  # create ggplot2 mapping to geometries
  if (order == TRUE) {
    p <- ggplot2::ggplot(
      dt, ggplot2::aes(x = reorder(.data$targetx, -.data$targety),
                       y = .data$targety))
    if (fill == FALSE) {
      p <- p + ggplot2::geom_bar(stat = "identity", fill = fillc)

    } else if (fill == TRUE) {
      p <- p + ggplot2::geom_bar(
        ggplot2::aes(fill = reorder(.data$targetx, .data$targety)),
        stat = "identity"
        ) +
        ggplot2::guides(fill = ggplot2::guide_legend(title = x))
    }
  } else if (order == FALSE) {
    p <- ggplot2::ggplot(
      dt, ggplot2::aes(x = .data$targetx, y = .data$targety)
      )
    if (fill == FALSE) {
      p <- p + ggplot2::geom_bar(stat = "identity", fill = fillc)

    } else if (fill == TRUE) {
      p <- p + ggplot2::geom_bar(
        ggplot2::aes(fill = reorder(.data$targetx, .data$targety)),
        stat = "identity"
        ) +
        ggplot2::guides(fill = ggplot2::guide_legend(title = x))
    }
  }
  # add scales definitions and labels
  if (mode == "count") {
    p <- p +
      ggplot2::scale_y_continuous(
        limits = c(0, max_num + (max_num / 20))
      )
    if (flip == FALSE) {
      if (theme == "light") {
         p <- p +
          ggplot2::geom_text(
            ggplot2::aes(label = .data$targety),
            vjust = -1, fontface = "bold", color = "grey10"
          ) + light_theme(base_family, family)
      } else if (theme == "dark") {
        ddpcr::quiet(
          p <- p +
            ggplot2::geom_text(
              ggplot2::aes(label = .data$targety),
              vjust = -1, fontface = "bold", color = "grey90"
            ) + dark_theme(base_family, family),
        all = TRUE
        )
      }
    } else if (flip == TRUE) {
      if (theme == "light") {
        p <- p +
          ggplot2::geom_text(
            ggplot2::aes(label = .data$targety),
            vjust = .5, hjust = -.2, fontface = "bold", color = "grey10"
          ) +  light_theme(base_family, family) +
          coord_flip()
      } else if (theme == "dark") {
        ddpcr::quiet(
          p <- p +
            ggplot2::geom_text(
              ggplot2::aes(label = .data$targety),
              vjust = .5, hjust = -.2, fontface = "bold", color = "grey90"
            ) + dark_theme(base_family, family) +
            coord_flip(),
          add = TRUE
        )
      }
    }
  } else if (mode == "prop") {
    p <- p +
      ggplot2::scale_y_continuous(
        limits = c(0, max_num + (max_num / 20)),
        labels = scales::percent
      )
    if (flip == FALSE) {
      if (theme == "light") {
        p <- p +
          ggplot2::geom_text(
            ggplot2::aes(label = scales::percent(.data$targety, accuracy = .1)),
            vjust = -1, fontface = "bold", color = "grey10"
          ) + light_theme(base_family, family)
      } else if (theme == "dark") {
        ddpcr::quiet(
          p <- p +
            ggplot2::geom_text(
              ggplot2::aes(label = scales::percent(.data$targety, accuracy = .1)),
              vjust = -1, fontface = "bold", color = "grey90"
            ) + dark_theme(base_family, family),
          all = TRUE
        )
      }
    } else if (flip == TRUE) {
      if (theme == "light") {
        p <- p +
          ggplot2::geom_text(
            ggplot2::aes(label = scales::percent(.data$targety, accuracy = .1)),
            vjust = .5, hjust = -.2, fontface = "bold", color = "grey10"
          ) +  light_theme(base_family, family) +
          coord_flip()
      } else if (theme == "dark") {
        ddpcr::quiet(
          p <- p +
            ggplot2::geom_text(
              ggplot2::aes(label = scales::percent(.data$targety, accuracy = .1)),
              vjust = .5, hjust = -.2, fontface = "bold", color = "grey90"
            ) + dark_theme(base_family, family) +
            coord_flip(),
          all = TRUE
        )
      }
    }
  }
  if (theme == "dark"){
    ggdark::invert_geom_defaults()
  }
  # add labs
  p <- p + ggplot2::labs(title = title, subtitle = subtitle, x = NULL, y = NULL)
  # output
  return(p)
}
