.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    paste("Welcome to Simple Exploratory Data Anlaysis with seda R package version",
          utils::packageVersion("seda")))
}


.onLoad <- function(libname,pkgname) {
  pdfFonts <- grDevices::pdfFonts
  extrafont::loadfonts("pdf",quiet=T)
}
