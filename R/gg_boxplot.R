#' Produire un boxplot sur les stations ou les observations
#'
#' @param df Dataframe contenant les données.
#' @param var_quant Caractère. Nom de la variable quantitative à représenter (ordonnée).
#' @param var_quant_lab Caractère. Etiquette de l'axe des y.
#' @param var_qual Caractère. Nom de la variable qualitative à représenter (abscisse).
#' @param var_qual_lab Caractère. Etiquette de l'axe des x.
#' @param couleur Caractère. Couleur de remplissage des barres de l'histogramme.
#'
#' @return Un boxplot ggplot2.
#' @export
#' @importFrom dplyr filter pull
#' @importFrom ggplot2 ggplot geom_boxplot labs aes theme element_text
#'
#' @examples \dontrun{
#' gg_boxplot(
#'   df = data,
#'   var_quant = "voltage",
#'   var_quant_lab = "Voltage (V)",
#'   var_qual = "cran",
#'   var_qual_lab = "Cran de réglage du Héron"
#' )
#' }
gg_boxplot <- function(df,
                       var_quant,
                       var_quant_lab,
                       var_qual,
                       var_qual_lab,
                       couleur = "#7570B3")
{

  df <- df %>%
    filter(!is.na(get(var_quant)) &
           !is.na(get(var_qual)))

  quantiles <- df %>%
    pull(var_quant) %>%
    quantile(probs = c(0.1, 0.9))

  ggplot(data = df,
         aes(x = as.factor(get(var_qual)),
             y = get(var_quant))) +
    geom_boxplot(fill = couleur#,
               #  outlier.shape = NA
                 ) +
    labs(x = var_qual_lab,
         y = var_quant_lab) +
    theme(text = element_text(size = 20)) #+
   # scale_y_continuous(limits = quantiles)

}
