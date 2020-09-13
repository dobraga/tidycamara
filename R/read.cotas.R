#' read.cotas
#'
#' @export
#'
read.cotas <- function(dir=tempdir()){
  files <- paste0(dir, "/", list.files(dir, "^Ano.+csv$"))

  df <- purrr::map_df(
    files,
    ~ readr::read_delim(
      .x, delim = ";",
      col_types = readr::cols(
        .default = readr::col_character(),
        datEmissao = readr::col_datetime(format = ""),
        vlrDocumento = readr::col_double(),
        vlrGlosa = readr::col_double(),
        vlrLiquido = readr::col_double(),
        numMes = readr::col_double(),
        numAno = readr::col_double(),
        vlrRestituicao = readr::col_double()
      )
    )
  ) %>%
    dplyr::mutate(
      vlrFinal = dplyr::if_else(vlrLiquido > 0, vlrLiquido, vlrRestituicao),
      dt_anomes = lubridate::parse_date_time(paste(numAno, numMes, sep = "-"), "Y-m")
    )

  return(df)
}
