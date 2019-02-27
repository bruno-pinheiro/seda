#' seda: A package for make exploratory data analysis tasks simple
#'
#' The seda package provides some cateogories of functions,
#' which makes easy to produce and visualize descriptive statistics
#' and exploratory analysis tasks.
#'
#' It's a \code{tidyverse} based package. Data are imported with
#' \code{readr} and manipulated as \code{tibble}, using pipelines.
#' Plots are made with \code{ggplot2}
#'
#' Beyound that, it has some functions to retrieve brazilian Census data
#' from IBGE.
#'
#' It's still in development and is more a personal package than a powerfull
#' tool builded for the public. But if makes my life easier, could do the same
#' for you.
#'
#' It's tested with \code{devtools} and \strong{shared versions will always be
#' errors/warnings/notes-free (in my machine, a linnux Debian 9). It wasn't
#' tested in another systems}.
#'
#' I would appreaciate any feedback. A lot!
#'
#' Enjoy by your own risk.
#'
#' @section Descriptie functions:
#'
#' \describe{
#' Function to visualize data:
#'
#' \itemize{
#'   \item \code{plot_na}
#'   \item \code{plot_bar}
#'   \item \code{plot_bar_id}
#' }
#' }
#'
#' @section Get data functions:
#'
#' \describe{
#' Function to download and import IBGE data
#'
#' \itemize{
#'   \item \code{get_ibge}
#'   \item \code{read_ibge}
#'   \item \code{read_geoibge}
#' }
#' }
#'
#' @section File manipulation functions:
#' Functions to manipulate files on temporary folder
#'
#' \describe{
#' \itemize{
#'   \item \code{list_tmp}
#'   \item \code{clean_tmp}
#' }
#' }
#'
#' @docType package
#' @name seda
"_PACKAGE"
