#' Mise en forme des popups ggplotly
#'
#' @param g_plotly Graphique de classe plotly
#'
#' @return Le graphique dont les popups ont été modifiés.
#' @export
#'
#' @importFrom stringr str_replace_all
#'
#' @examples
gp_mef_survol <- function(g_plotly)

{
  mef_survol_int <- function(texte)

  {
    texte <- texte %>%
      str_replace_all(pattern = "station:",
                      replacement = "Station :") %>%
      str_replace_all(pattern = "conductivite:",
                      replacement = "Conductivité :") %>%
      str_replace_all(pattern = "penny_15_centre:",
                      replacement = "Penny à 1,5m :") %>%
      str_replace_all(pattern = "cran",
                      replacement = "Cran ") %>%
      str_replace_all(pattern = "annee",
                      replacement = "Année ")
  }

  for (i in 1:length(g_plotly)) {
    g_plotly$x$data[[i]]$text <- g_plotly$x$data[[i]]$text %>%
      mef_survol_int()
  }

  g_plotly

}
