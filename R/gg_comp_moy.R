#' Produire un diagramme barre d'erreur pour comparer les années
#'
#' @param df Caractère. Nom du dataframe contenant les données.
#' @param var Caractère. Nom de la variable quantitative à représenter (ordonnée).
#' @param y_lab Caractère. Etiquette de l'axe des y.
#' @param couleur Caractère. Couleur de remplissage des barres.
#'
#' @return Un errorbar ggplot2.
#' @export
#' @importFrom dplyr group_by summarise n pull
#' @importFrom ggplot2 ggplot geom_pointrange geom_bar geom_hline labs aes theme element_text
#'
#' @examples
gg_comp_moy <- function(df,
                        var,
                        y_lab = NA,
                        couleur = "#7570B3")

  {

  if(is.na(y_lab)) y_lab <- var

  df %>%
    group_by(annee) %>%
    summarise(
      et = sd(get(var), na.rm = T),
      moy = mean(get(var), na.rm = T),
      n = n(),
      ymin = moy - et / n ^ 0.5,
      ymax = moy + et / n ^ 0.5
    ) %>%
    ggplot(aes(
      x = as.factor(annee),
      y = moy,
      fill = as.factor(annee)
    )) +
    geom_bar(stat = "Identity",
             fill = couleur) +
    geom_pointrange(aes(ymin = ymin, ymax = ymax),
                    colour = "black") +
    geom_hline(
      yintercept = df %>%
        pull(get(var)) %>%
        mean(na.rm = T),
      linetype = "longdash",
      size = 0.7,
      col = "limegreen"
    ) +
    theme(legend.position = "none",
          text = element_text(size = 20)) +
    labs(title = "Moyenne annuelle",
         x = "",
         y = y_lab)

}
