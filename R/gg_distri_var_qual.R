# -----------------------------------------------------------------------
#' Graphique de distribution d'une variable qualitative
#'
#' @param df Nom du dataframe contenant les données.
#' @param var Caractère. Nom de la variable qualitative à représenter.
#' @param xlab Caractère. Etiquette de l'axe des x.
#' @param type Caractère. Type de dataframe, soit "pêches", soit "stations".
#' @param couleur Caractère. Couleur de remplissage des barres.
#'
#' @return Un diagramme ggplot2.
#' @export
#'
#' @examples
gg_distri_var_qual <- function(df,
                               var,
                               xlab,
                               type = "stations",
                               couleur = "#D95F02") {

  if (type == "stations") {
    df <- df %>%
      select(station, var) %>%
      unique()}

  if (type == "stations") {ylab <- "Nb de stations"
  } else {
    ylab <- "Nb de pêches"}

  df %>%
    pull(get(var)) %>%
    table() %>%
    as.data.frame() %>%
    magrittr::set_colnames(c(var, "n_obs")) %>%
    ggplot(aes(x = as.factor(get(var)),
               y = n_obs)) +
    geom_bar(stat = 'Identity',
             fill = '#7570B3') +
    labs(x = xlab,
         y = ylab) +
    theme(text = element_text(size = 20))

}
