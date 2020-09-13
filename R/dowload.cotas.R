#' download.cotas
#'
#' @export
#'
download.cotas <- function(ano=2020, dir=tempdir()){
  filename <- paste0("Ano-", ano, ".csv.zip")
  destfile <- paste0(dir, "/", filename)
  url <- paste0("http://www.camara.leg.br/cotas/", filename)

  purrr::walk2(url, destfile, ~download.file(url = .x, destfile = .y))
  purrr::walk(destfile, ~unzip(.x, exdir = dir))
}
