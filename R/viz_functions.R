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
#' @return If \code{varcut} is not passed, then the function plot a simple
#'         stacked barplot showing the counts of NAs for each variable in
#'         the dataset. If is, then the counts are faceted.
#'
#' @examples
#' # plot_na without varcut
#' plot_na(df)
#'
#' # plot_na with varcut
#' plot_na(df, varcut = "cut")
#'
#' # plot_na manipulation other parameters
#' plot_na(df, varcut = "e", "Counts of NAs", c("yellow", "purple"))
#'
#' @import ggplot2 ggdark
#' @importFrom dplyr if_else
#' @importFrom magrittr "%>%"
#' @importFrom rlang enquo
#'
#' @export
plot_na <- function(df, datecut = NULL, title = NULL,
                    colors = c("purple3", "yellow")){
  if (is.null(datecut)){
    return(
      df %>%
        tibble::as_tibble() %>%
        dplyr::mutate_all(as.character) %>%
        tidyr::gather(key, value) %>%
        dplyr::mutate(key = factor(key, levels = rev(names(df)))) %>%
        dplyr::group_by(key) %>%
        dplyr::count(na = is.na(value)) %>%
        dplyr::mutate(na = if_else(na == TRUE, "Sim", "Não")) %>%
        ggplot2::ggplot(aes(x = key, y = n, fill = na)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::scale_fill_manual(values = colors, "Valor é NA?") +
        ggplot2::labs(title = title, x = NULL, y = NULL) +
        ggplot2::theme(axis.text.x = element_blank(),
                       legend.position = "top") +
        ggplot2::coord_flip() +
        dark_theme() +
        theme(axis.text.x = element_blank())
    )
  } else {

    date_cut <- enquo(datecut)
    return(
      df %>%
        tibble::as_tibble() %>%
        dplyr::rename_("varcut" = datecut) %>%
        dplyr::mutate_all(as.character) %>%
        tidyr::gather(key, value, - varcut) %>%
        dplyr::mutate(key = factor(key, levels = rev(names(df)))) %>%
        dplyr::group_by(varcut, key) %>%
        dplyr::count(na = is.na(value)) %>%
        dplyr::mutate(na = if_else(na == TRUE, "Sim", "Não")) %>%
        ggplot2::ggplot(aes(x = key, y = n, fill = na)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::scale_fill_manual(values = colors, "Valor é NA?") +
        ggplot2::facet_wrap(~varcut, scales = "free_x" ,
                            ncol = df %>% dplyr::pull(!!date_cut) %>% unique %>% length) +
        ggplot2::labs(title = title, x = NULL, y = NULL) +
        ggplot2::theme(axis.text.x = element_blank(), legend.position = "top") +
        ggplot2::coord_flip() +
        dark_theme() +
        theme(axis.text.x = element_blank())
    )
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
#' plots <- lapply(vars, function(i) plot_bar(df, i))
#'
#' @import dplyr ggplot2 sf
#'
#' @export
plot_bar <- function(df, x, fill = NULL) {
  x <- rlang::sym(x)
  prop <- NULL

  if(is.null(fill)) {
    maxProp <-
      df %>%
      filter(!is.na(!!x)) %>%
      count(!!x) %>%
      mutate(prop = prop.table(n)) %>%
      summarise(maxProp = max(prop) + .08) %>%
      as.numeric()

    df %>%
      as.data.frame() %>%
      filter(!is.na(!!x)) %>%
      count(!!x) %>%
      mutate(prop = prop.table(n)) %>%
      ggplot(aes(y = prop, x = !!x)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(labels = scales::percent, limits = c(0, maxProp)) +
      geom_text(aes(label = scales::percent(round(prop, 3))), hjust=-.1) +
      labs(title=paste0("Distribuicao de proporcoes para ", colnames(select(df, !!x))),
           x = NULL, y = NULL) +
      light_theme() +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) +
      coord_flip()
  } else {
    fill <- rlang::sym(fill)
    maxProp <-
      df %>%
      filter(!is.na(!!x)) %>%
      # group_by(!!fill) %>%
      count(!!x) %>%
      mutate(prop = prop.table(n)) %>%
      summarise(maxProp = max(prop) + .08) %>%
      as.numeric()

    df %>%
      as.data.frame() %>%
      filter(!is.na(!!x)) %>%
      group_by(!!fill) %>%
      count(!!x) %>%
      mutate(prop = prop.table(n)) %>%
      ggplot(aes(y = prop, x = !!x, fill = !!fill)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_text(aes(label = scales::percent(round(prop, 3))),
                position = position_dodge(width = .9), hjust=-.1) +
      scale_y_continuous(labels = scales::percent, limits = c(0, maxProp)) +
      labs(title=paste0("Distribuicao de proporcoes para ", colnames(select(df, !!x))),
           x = NULL, y = NULL) +
      light_theme() +
      theme(panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            legend.position = "top") +
      coord_flip()
  }
}

