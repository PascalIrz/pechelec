#' Produire un diagramme bivarié par cran de réglage
#'
#' @param df Dataframe contenant les données.
#' @param x Caractère. Nom de la variable à représenter en abscisse.
#' @param y Caractère. Nom de la variable à représenter en ordonnée
#'
#' @return Le graphique ggplot2.
#' @export
#'
#' @importFrom purrr map
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth labs scale_y_continuous theme element_text
#' @importFrom ggplot2 annotate scale_color_manual
#' @importFrom scales number_format
#'
#' @examples
gg_bivar_cran <- function(df, x, y)

{
  # Equations mises en forme
  labels <- map(.x = (2:4),
                .f = int_equation_cran,
                df = prov,
                x = x,
                y = y)

  # graphique
  ggplot(data = prov,
         aes(
           x = get(x),
           y = get(y),
           col = cran,
           label = station,
           text = annee
         )) +
    geom_point() +
    geom_smooth(method = 'lm',
                se = FALSE) +
    labs(x = "Conductivité (\U00B5S/cm)",
         y = "Intensité (A)",
         col = "Cran") +
    scale_y_continuous(labels = number_format(accuracy = 1)) +
    theme(text = element_text(size = 15)) +
    annotate(
      geom = "text",
      x = -Inf,
      y = Inf,
      label = labels,
      col = couleurs[1:3],
      hjust = rep((-0.1), 3),
      vjust = 1.5*(1:3)
    ) +
    scale_color_manual(values = couleurs)

}
