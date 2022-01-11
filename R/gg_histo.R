#' Produire l'histogramme d'une des variables du jdd
#'
#' @param df Caractère. Nom du dataframe contenant les données.
#' @param var Caractère. Nom de la variable à représenter.
#' @param x_lab Caractère. Etiquette de l'axe des x.
#' @param type Caractère. Type de dataframe, soit "pêches", soit "stations".
#' @param couleur Caractère. Couleur de remplissage des barres de l'histogramme.
#'
#' @return Un histogramme ggplot.
#' @export
#' @importFrom dplyr select pull
#' @importFrom ggplot2 ggplot geom_histogram labs geom_vline aes theme element_text scale_y_continuous
#' @importFrom scales number_format
#'
#' @examples
gg_histo <- function(df,
                     var,
                     x_lab,
                     type = "stations",
                     couleur = "#D95F02")
{
  if (type == "stations") {
    df <- df %>%
      select(station, var) %>%
      unique()
  }

  if (type == "stations") {
    y_lab <- "Nb de stations"
  } else {
    y_lab <- "Nb de pêches"
  }

  moy <- df %>%
    pull(var) %>%
    mean(na.rm = TRUE)

  range <- df %>%
    pull(var) %>%
    range(na.rm = TRUE)

  amp <- range[2] - range [1]

  label = paste0("moyenne : ",
                 round(moy, digits = 2))

  ggplot(data = df,
         aes(x = get(var))) +
    geom_histogram(bins = 10,
                   fill = couleur) +
    labs(x = paste0(x_lab, " - ", label),
         y = y_lab) +
    geom_vline(
      xintercept = moy,
      linetype = "longdash",
      size = 0.7,
      col = "limegreen"
    ) +
    theme(text = element_text(size = 20)) +
    scale_y_continuous(labels = number_format(accuracy = 1))

}
