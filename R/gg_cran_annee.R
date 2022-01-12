#' Produire un diagramme pour comparer les années sur le cran de réglage et la conductivité
#'
#' @param df Dataframe contenant les données.
#' @param var_ordre Caractère. Nom de la variable servant à ordonner les stations en ordonnée.
#'
#' @return Un diagramme ggplot2.
#' @export
#' @importFrom dplyr mutate group_by summarise
#' @importFrom ggplot2 ggplot geom_point labs aes guides guide_legend scale_radius
#'
#' @examples
gg_cran_annee <- function(df, var_ordre)

{

  ordre <- df %>%
    group_by(station) %>%
      summarise(moy = mean(get(var_ordre), na.rm = T))

  df <- df %>%
    left_join(ordre) %>%
    mutate(station = as.factor(station),
           station = fct_reorder(station, moy))

  df %>%
    mutate(cran = as.integer(cran)) %>%
    ggplot(aes(x = annee,
               y = station)) +
    geom_point(aes(size = cran,
                   col = conductivite)) +
    labs(x = "",
         y = "",
         col = "Conductivit\u00e9") +
    scale_radius(
      range  = c(2, 6),
      name   = "Cran",
      breaks = c(1, 2, 3),
      labels = 1:3
    ) +
    scale_color_gradient(low = "green", high = "red") +
    guides(size = guide_legend(override.aes = list(colour = "red")))
}
