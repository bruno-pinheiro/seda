#' @title Get tabular data from IBGE
#' @name get_ibge()
#'
#' @description Download and extract tabular data from ftp://ftp.ibge.gov.br/
#'
#' @param url a string defining url of zipped shapfile to get
#'
#' @details \code{get_ibge} will download and extract files in a
#'          \code{tempdir()}. Files keep stored temporarily. To get other
#'          \code{.zip} is needed to remove files from \code{tempdir()}: see
#'          \code{\link{clean_tmp}}.
#'
#' @return extrated files in temporary directory
#'
#' @author Bruno Pinheiro
#'
#' @examples
#' u1 <- "ftp://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/"
#' u2 <- "malhas_de_setores_censitarios__divisoes_intramunicipais/censo_2010/"
#' u3 <- "setores_censitarios_shp/al/al_distritos.zip"
#' get_ibge(paste0(u1, u2, u3))
#' clean_tmp()
#'
#' @importFrom utils download.file unzip
#'
#' @export
get_ibge <- function(url) {
  if (!file.exists(tempdir())) { dir.create(tempdir()) }
  tmp <- tempfile(fileext = ".zip")
  download.file(url, destfile = tmp)
  unzip(tmp, exdir = tempdir(), junkpaths = TRUE)
  message(
    paste("File", gsub(".*/", "", url),
          "downloaded, unziped and imported from http://ftp.ibge.gov.br.",
          "Files are on", tempdir(), "for imports. Use list_tmp() to list",
          "file names, read_ibge() and read_geoibge() to import your desired",
          "data on R and clean_tmp() to close temporary connections and",
          "delete temporary files.\nATTENTION: after clean_tmp() files need",
          "to be downloaded again.")
  )
}


#' @title Import tabular data from IBGE
#' @name read_ibge()
#'
#' @description Import tabular data (for now just \code{.csv} format) from
#'              \code{tempdir()}.
#'
#' @param file a string defining the file name to import
#'
#' @details \code{read_ibge} will import informed file from \code{tempdir()}
#'          directory. Files keep stored temporarily. To remove files from
#'          \code{tmpdir}, see \code{\link{clean_tmp}}.
#'
#' @return extrated files in temporary directory
#'
#' @author Bruno Pinheiro
#'
#' @examples
#' u1 <- "ftp://ftp.ibge.gov.br/Censos/Censo_Demografico_2010/"
#' u2 <- "Resultados_do_Universo/Agregados_por_Setores_Censitarios/RR_20171016.zip"
#' get_ibge(paste0(u1, u2))
#' st <-read_ibge("Pessoa07_RR.csv")
#' clean_tmp()
#'
#' @export
read_ibge <- function(file) {
  ddpcr::quiet(
    tb <- readr::read_delim(file.path(tempdir(), file), delim = ";")
    )
  return(tb)
  }


#' @title Get spatial data from IBGE
#' @name read_geoibge()
#'
#' @description Download, extract and import zipped shapefiles from
#'              ftp://geoftp.ibge.gov.br/
#'
#' @param layer a string defining url of zipped shapfile to get
#' @param epsg a numeric value defining projection epsg code (default is 31983)
#'
#' @details \code{read_geoibge} will download and extract files in a temp
#'          directory. Files keep stored temporarily. To remove files from
#'          \code{tmpdir}, see \code{\link{clean_tmp}}.
#'
#' @return extrated files in temporary directory
#'
#' @author Bruno Pinheiro
#'
#' @examples
#' u1 <- "ftp://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/"
#' u2 <- "malhas_de_setores_censitarios__divisoes_intramunicipais/censo_2010/"
#' u3 <- "setores_censitarios_shp/al/al_municipios.zip"
#' get_ibge(paste0(u1, u2, u3))
#' layer <- list_tmp(".shp", full = FALSE)
#' layer <- gsub("\\.shp", "", layer)
#' st <- read_geoibge(layer)
#' clean_tmp()
#'
#' @export
read_geoibge <- function(layer, epsg = 31983) {
  tmpdir <- tempdir()
  st <- sf::read_sf(dsn = tmpdir, layer = layer, options = "ENCODING=Latin1")
  st <- sf::st_transform(st, epsg)
  return(st)
}


