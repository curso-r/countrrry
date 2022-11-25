#' Calcular estatisticas
#'
#' Essa função é usada para calcular estatísticas de população e
#' expectativa de vida, segundo os continentes e ano. Ela utiliza a base
#' gapminder.
#'
#'
#' @param continentes um vetor com o nome do(s) continente(s)
#' @param anos um vetor com os anos. o padrão é de 2000 até 2022.
#'
#' @return uma tibble
#' @export
#'
#' @examples calcular_estatisticas("África")
calcular_estatisticas <- function(continentes, anos = 2000:2022) {
  if (missing(continentes)) usethis::ui_stop("PRECISO DE UM CONTINENTE")


  if(!continentes %in% unique(dados::dados_gapminder$continente)){
    usethis::ui_stop(
      glue::glue("Indique um continente dentre os seguintes:
      {paste0(unique(dados::dados_gapminder$continente), collapse = ', ')}")
    )
  }


  dados::dados_gapminder |>
   # dplyr::filter(.data[["continente"]] %in% continentes, ano %in% anos) |>
    dplyr::filter(continente %in% continentes, ano %in% anos) |>
    dplyr::group_by(ano) |>
    dplyr::summarise(
      pop = sum(populacao) / 1e6,
      vida = mean(expectativa_de_vida)
    )
}
