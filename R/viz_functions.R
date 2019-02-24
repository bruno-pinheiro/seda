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
#' @param mode a string defining theme: "light" (default) or "dark"
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
#' # plot_na with varcut
#' plot_na(sample_data, varcut = "cut_var")
#'
#' # plot_na manipulating other parameters
#' plot_na(
#'   sample_data,
#'   varcut = "cut_var",
#'   title = "Counts of NAs",
#'   mode = "dark",
#'   colors = c("red", "darkgreen")
#'   )
#'
#' @importFrom magrittr "%>%"
#'
#' @export
plot_na <- function(df, datecut = NULL, title = NULL,
                    mode = "light", colors = c("purple3", "yellow")){
  if (mode == "light") {
    if (is.null(datecut)){
      p <- df %>%
        tibble::as_tibble() %>%
        dplyr::mutate_all(as.character) %>%
        tidyr::gather(key, value) %>%
        dplyr::mutate(key = factor(key, levels = rev(names(df)))) %>%
        dplyr::group_by(key) %>%
        dplyr::count(na = is.na(value)) %>%
        dplyr::mutate(na = dplyr::if_else(na == TRUE, "Sim", "Não")) %>%
        ggplot2::ggplot(aes(x = key, y = n, fill = na)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::scale_fill_manual(values = colors, "Valor é NA?") +
        ggplot2::labs(title = title, x = NULL, y = NULL) +
        ggplot2::theme(axis.text.x = element_blank(),
                       legend.position = "top") +
        light_theme() +
        theme(axis.text.x = element_blank(),
              legend.position = "top") +
        ggplot2::coord_flip()
      return(p)
    } else {
      date_cut <- rlang::enquo(datecut)
      p <- df %>%
        tibble::as_tibble() %>%
        dplyr::rename_("varcut" = datecut) %>%
        dplyr::mutate_all(as.character) %>%
        tidyr::gather(key, value, - varcut) %>%
        dplyr::mutate(key = factor(key, levels = rev(names(df)))) %>%
        dplyr::group_by(varcut, key) %>%
        dplyr::count(na = is.na(value)) %>%
        dplyr::mutate(na = dplyr::if_else(na == TRUE, "Sim", "Não")) %>%
        ggplot2::ggplot(ggplot2::aes(x = key, y = n, fill = na)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::scale_fill_manual(values = colors, "Valor é NA?") +
        ggplot2::facet_wrap(
          ~varcut, scales = "free_x" ,
          ncol = df %>% dplyr::pull(!!date_cut) %>% unique %>% length
          ) +
        ggplot2::labs(title = title, x = NULL, y = NULL) +
        light_theme() +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                       legend.position = "top") +
        ggplot2::coord_flip()
      return(p)
    }
  } else if (mode == "dark") {
    if (is.null(datecut)){
      p <- df %>%
        tibble::as_tibble() %>%
        dplyr::mutate_all(as.character) %>%
        tidyr::gather(key, value) %>%
        dplyr::mutate(key = factor(key, levels = rev(names(df)))) %>%
        dplyr::group_by(key) %>%
        dplyr::count(na = is.na(value)) %>%
        dplyr::mutate(na = dplyr::if_else(na == TRUE, "Sim", "Não")) %>%
        ggplot2::ggplot(ggplot2::aes(x = key, y = n, fill = na)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::scale_fill_manual(values = colors, "Valor é NA?") +
        ggplot2::labs(title = title, x = NULL, y = NULL) +
        dark_theme() +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                       legend.position = "top") +
        ggplot2::coord_flip()
      ggdark::invert_geom_defaults()
      message("ATENTION: Already changed back inside seda::plot_na")
      return(p)
      } else {
        date_cut <- rlang::enquo(datecut)
        p <- df %>%
          tibble::as_tibble() %>%
          dplyr::rename_("varcut" = datecut) %>%
          dplyr::mutate_all(as.character) %>%
          tidyr::gather(key, value, - varcut) %>%
          dplyr::mutate(key = factor(key, levels = rev(names(df)))) %>%
          dplyr::group_by(varcut, key) %>%
          dplyr::count(na = is.na(value)) %>%
          dplyr::mutate(na = dplyr::if_else(na == TRUE, "Sim", "Não")) %>%
          ggplot2::ggplot(ggplot2::aes(x = key, y = n, fill = na)) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::scale_fill_manual(values = colors, "Valor é NA?") +
          ggplot2::facet_wrap(
            ~varcut, scales = "free_x" ,
            ncol = df %>% dplyr::pull(!!date_cut) %>% unique %>% length
            ) +
          ggplot2::labs(title = title, x = NULL, y = NULL) +
          dark_theme() +
          ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                         legend.position = "top") +
          ggplot2::coord_flip()
        ggdark::invert_geom_defaults()
        message("ATENTION: Already changed back inside seda::plot_na")
        return(p)
      }
  }
}

#' @title Bar plot for univariate analysis
#' @name plot_bar
#'
#' @description It counts a categorical variable and plots a publication
#'              ready ggplot2 barplot
#'
#' @param df a data frame
#' @param x a string indicating a categorical variable
#' @param fill a string indicating a categorical variable
#' @param flip a logical TRUE or FALSE
#' @param mode a string defining theme: "light" (default) or "dark"
#'
#' @details Uses dplyr and ggplot
#'
#' @return Um grafico de barras para a variavel \code{x} da base \code{df}.
#'
#' @author Bruno Pinheiro
#'
#' @seealso \code{\link[ggplot2]{geom_bar()}},
#'          \code{\link[gridExtra]{grid.arrange()}},
#'          \code{\link[scales]{percent()}}
#'
#' @examples
#' # simple barplot
#' plot_var(var5)
#'
#' # plot more than one variable at once
#' vars <- c("var5", "cut_var")
#' plots <- lapply(vars, function(i) plot_bar(sample_data, i))
#'
#' @export
plot_bar <- function(df, x, fill = NULL, flip = FALSE, mode = "light") {
  df <- df %>% tibble::as_tibble %>% dplyr::rename_("target" = x)
  maxProp <-
    df %>%
    dplyr::count(target) %>%
    dplyr::mutate(prop = prop.table(n)) %>%
    dplyr::summarise(maxProp = max(prop)) %>%
    as.numeric()

  if (mode == "light") {
    if (flip == FALSE) {
      if (is.null(fill)) {
        p <- df %>%
          dplyr::count(target) %>%
          dplyr::mutate(prop = prop.table(n)) %>%
          ggplot2::ggplot(ggplot2::aes(y = prop, x = target)) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::scale_y_continuous(
            labels = scales::percent,
            limits = c(0, maxProp + (maxProp / 10)),
            breaks = seq(0, maxProp + (maxProp / 10), maxProp / 5)
          ) +
          ggplot2::geom_text(
            ggplot2::aes(label = scales::percent(round(prop, 3))),
            vjust = -.5, fontface = "bold"
          ) +
          ggplot2::labs(title = paste("Frequências relativas de", x),
                        x = NULL, y = NULL) +
          light_theme()
        return(p)
      } else {
        p <- df %>%
          dplyr::count(target) %>%
          dplyr::mutate(prop = prop.table(n)) %>%
          ggplot2::ggplot(ggplot2::aes(y = prop, x = target, fill = target)) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::scale_y_continuous(
            labels = scales::percent,
            limits = c(0, maxProp + (maxProp / 10)),
            breaks = seq(0, maxProp + (maxProp / 10), maxProp / 5)
          ) +
          ggplot2::geom_text(
            ggplot2::aes(label = scales::percent(round(prop, 3))),
            vjust = -.5, fontface = "bold"
          ) +
          ggplot2::labs(title = paste("Frequências relativas de", x),
                        x = NULL, y = NULL) +
          light_theme()
        return(p)
      }
    } else if (flip == TRUE) {
      if (is.null(fill)) {
        p <- df %>%
          dplyr::count(target) %>%
          dplyr::mutate(prop = prop.table(n)) %>%
          ggplot2::ggplot(ggplot2::aes(y = prop, x = target)) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::scale_y_continuous(
            labels = scales::percent,
            limits = c(0, maxProp + (maxProp / 10)),
            breaks = seq(0, maxProp + (maxProp / 10), maxProp / 5)
          ) +
          ggplot2::geom_text(
            ggplot2::aes(label = scales::percent(round(prop, 3))),
            vjust = .5, hjust = -.2, fontface = "bold"
          ) +
          ggplot2::labs(title = paste("Frequências relativas de", x),
                        x = NULL, y = NULL) +
          light_theme() +
          coord_flip()
        return(p)
      } else {
        p <- df %>%
          dplyr::count(target) %>%
          dplyr::mutate(prop = prop.table(n)) %>%
          ggplot2::ggplot(ggplot2::aes(y = prop, x = target, fill = target)) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::scale_y_continuous(
            labels = scales::percent,
            limits = c(0, maxProp + (maxProp / 10)),
            breaks = seq(0, maxProp + (maxProp / 10), maxProp / 5)
          ) +
          ggplot2::geom_text(
            ggplot2::aes(label = scales::percent(round(prop, 3))),
            vjust = .5, hjust = -.2, fontface = "bold"
          ) +
          ggplot2::labs(title = paste("Frequências relativas de", x),
                        x = NULL, y = NULL) +
          light_theme() +
          coord_flip()
        return(p)
      }
    }
  } else if (mode == "dark") {
    if (flip == FALSE) {
      if (is.null(fill)) {
        p <- df %>%
          dplyr::count(target) %>%
          dplyr::mutate(prop = prop.table(n)) %>%
          ggplot2::ggplot(ggplot2::aes(y = prop, x = target)) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::scale_y_continuous(
            labels = scales::percent,
            limits = c(0, maxProp + (maxProp / 10)),
            breaks = seq(0, maxProp + (maxProp / 10), maxProp / 5)
          ) +
          ggplot2::geom_text(
            ggplot2::aes(label = scales::percent(round(prop, 3))),
            vjust = -.5, fontface = "bold", color = "grey"
          ) +
          ggplot2::labs(title = paste("Frequências relativas de", x),
                        x = NULL, y = NULL) +
          dark_theme()
        ggdark::invert_geom_defaults()
        message("ATENTION: Already changed back inside seda::plot_na")
        return(p)
      } else {
        p <- df %>%
          dplyr::count(target) %>%
          dplyr::mutate(prop = prop.table(n)) %>%
          ggplot2::ggplot(ggplot2::aes(y = prop, x = target, fill = target)) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::scale_y_continuous(
            labels = scales::percent,
            limits = c(0, maxProp + (maxProp / 10)),
            breaks = seq(0, maxProp + (maxProp / 10), maxProp / 5)
          ) +
          ggplot2::geom_text(
            ggplot2::aes(label = scales::percent(round(prop, 3))),
            vjust = -.5, fontface = "bold", color = "grey"
          ) +
          ggplot2::labs(title = paste("Frequências relativas de", x),
                        x = NULL, y = NULL) +
          dark_theme()
        ggdark::invert_geom_defaults()
        message("ATENTION: Already changed back inside seda::plot_na")
        return(p)
      }
    } else if (flip == TRUE) {
      if (is.null(fill)) {
        p <- df %>%
          dplyr::count(target) %>%
          dplyr::mutate(prop = prop.table(n)) %>%
          ggplot2::ggplot(ggplot2::aes(y = prop, x = target)) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::scale_y_continuous(
            labels = scales::percent,
            limits = c(0, maxProp + (maxProp / 10)),
            breaks = seq(0, maxProp + (maxProp / 10), maxProp / 5)
          ) +
          ggplot2::geom_text(
            ggplot2::aes(label = scales::percent(round(prop, 3))),
            vjust = .5, hjust = -.2, fontface = "bold", color = "grey"
          ) +
          ggplot2::labs(title = paste("Frequências relativas de", x),
                        x = NULL, y = NULL) +
          dark_theme() +
          coord_flip()
        ggdark::invert_geom_defaults()
        message("ATENTION: Already changed back inside seda::plot_na")
        return(p)
      } else {
        p <- df %>%
          dplyr::count(target) %>%
          dplyr::mutate(prop = prop.table(n)) %>%
          ggplot2::ggplot(ggplot2::aes(y = prop, x = target, fill = target)) +
          ggplot2::geom_bar(stat = "identity") +
          ggplot2::scale_y_continuous(
            labels = scales::percent,
            limits = c(0, maxProp + (maxProp / 10)),
            breaks = seq(0, maxProp + (maxProp / 10), maxProp / 5)
          ) +
          ggplot2::geom_text(
            ggplot2::aes(label = scales::percent(round(prop, 3))),
            vjust = .5, hjust = -.2, fontface = "bold", color = "grey"
          ) +
          ggplot2::labs(title = paste("Frequências relativas de", x),
                        x = NULL, y = NULL) +
          dark_theme() +
          coord_flip()
        ggdark::invert_geom_defaults()
        message("ATENTION: Already changed back inside seda::plot_na")
        return(p)
      }
    }
  }
}
