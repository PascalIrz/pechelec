# -----------------------------------------------------------------------
#' Graphique de distribution d'une variable qualitative
#'
#' @param df Nom du dataframe contenant les données.
#' @param var Caractère. Nom de la variable qualitative à représenter.
#' @param x_lab Caractère. Etiquette de l'axe des x.
#' @param type Caractère. Type de dataframe, soit "pêches", soit "stations".
#' @param couleur Caractère. Couleur de remplissage des barres.
#'
#' @return Un diagramme ggplot2.
#' @export
#'
#' @examples \dontrun{
#' gg_barre(
#'   df = data,
#'   var = protocole,
#'   x_lab = "Type de protocole"
#' )
#' }
gg_barre <- function(df,
                     var,
                     x_lab,
                     type = "stations",
                     couleur = "#D95F02")

  {

  var <- enquo(var)

  if (type == "stations") {
    df <- df %>%
      select(station, !!var) %>%
      unique()
  }

  if (type == "stations") {
    y_lab <- "Nb de stations"
  } else {
    y_lab <- "Nb de pêches"
  }

  df %>%
    count(!!var) %>%
    # table() %>%
    # as.data.frame() %>%
    # magrittr::set_colnames(c(var, "n_obs")) %>%
    ggplot(aes(x = as.factor(!!var),
               y = n)) +
    geom_bar(stat = 'Identity',
             fill = '#7570B3') +
    labs(x = x_lab,
         y = y_lab) +
    theme(text = element_text(size = 20))

}
